#!/usr/bin/env runhaskell
{-
  semtype.hs
  A Pandoc filter to enable human-readable typesetting for PL semantics.
-}

import Text.Pandoc.JSON
import Text.Regex
import System.IO


d :: Show a => a -> IO ()
d = hPutStrLn stderr . show


data Rewrite = Rewrite { pattern :: String
                       , replacement :: String
                       } deriving (Show)

sub :: Rewrite -> String -> String
sub (Rewrite {pattern = p, replacement = r}) input =
  subRegex (mkRegex p) input r


-- notation for sub-derivation in the proof tree
namedSubDerivationLast = Rewrite
  "(.*)[^-]--[[:blank:]]*([_[:alnum:]]*)\n---"
  "\\stackrel{\\2}{\\1}\n---"
namedSubDerivation = Rewrite
  "(.*)[^-]--[[:blank:]]*([_[:alnum:]]*)\n"
  "\\stackrel{\\2}{\\1} \\qquad\n"

-- horizontal bar for derivation
deriveFrom = Rewrite
  "((.*[^:=]\n)*)----*\n(.*\n)(.*)"
  "\\frac{\n\\1}{\\3} \\qquad \\4"

-- rule name (use \textsc)
rule = Rewrite
  "^([[:upper:]]+-[[:upper:]][[:alnum:]]*)"
  "\\textsc{\\1}"

-- derivation name (use \mathcal)
subDerivation s = foldl f s derivation
  where
    f s' p0 =
      subRegex (mkRegex $ "(^|[^\\[:alnum:]])"
                ++ p0 ++ "([^-[:alnum:]]|$)" -- do not touch rule names
               ) s' ("\\1\\mathcal{" ++ p0 ++ "}\\2")
-- list of derivation names
derivation = [ "E", "S", "SS", "T", "ST", "H" ]

-- special symbol
subSpecialSymbol s = foldl f s specialSymbol
  where
    f s' (p0, r0) =
      subRegex (mkRegex $ "(^|[^\\[:alnum:]])"
                ++ p0 ++ "([^[:alnum:]]|$)"
               ) s' ("\\1" ++ r0 ++ "\\2")
-- list of special symbols
specialSymbol = [
  -- Greek alphabet
  ("eta", "\\eta"),
  ("Gam", "\\Gamma"),
  ("lam", "\\lambda"),
  ("Phi", "\\Phi"),
  ("phi", "\\varphi"),
  ("Sig", "\\Sigma"),
  ("sig", "\\sigma"),
  ("tau", "\\tau"),
  -- data constructors
  ("inl", "\\textbf{inl}"),
  ("inr", "\\textbf{inr}"),
  -- types / sorts
  ("bool", "\\textbf{bool}"),
  ("int", "\\textbf{int}"),
  -- sets of numbers
  --("N", "\\mathbb{N}"),
  ("Z", "\\mathbb{Z}")
  --("Q", "\\mathbb{Q}"),
  --("R", "\\mathbb{R}"),
  -- math
  --("in", "\\in")
  ]

-- keyword text (use \textbf)
subKeywordText s = foldl f s keywordText
  where
    f s' p0 =
      subRegex (mkRegex $ "(^|[^\\[:alnum:]])"
                ++ p0 ++ "([^[:alnum:]]|$)"
               ) s' $ "\\1\\textbf{ " ++ p0 ++ " }\\2"
-- list of keyword texts
keywordText = [
  "Aexp", "Bexp", "Com",
  "true", "false",
  "if", "then", "else", "fi",
  "case", "of",
  "do", "while", "repeat", "until",
  "for", "to",
  "skip" ]

-- unique notation (that can be replaced context-freely)
subUniqueNotation s = foldl f s uniqueNotation
  where
    f s' (p0, r0) =
      subRegex (mkRegex p0) s' $ " " ++ r0 ++ " "
-- list of unique notations (in order of substitution!)
uniqueNotation = [
  ("&&", "\\land"),                  -- &&   (logical conjunction)
  ("\\|\\|", "\\lor"),               -- ||   (logical disjunction)
  ("!!", "\\neg"),                   -- !!   (logical negation)
  ("<==>", "\\iff"),                 -- <==> (logical equivalence, long)
  ("<=>", "\\Leftrightarrow"),       -- <=>  (logical equivalence)
  ("==>", "\\implies"),              -- ==>  (logical implication, long)
  ("=>", "\\Rightarrow"),            -- =>   (logical implication)
  ("\\|->", "\\mapsto"),             -- |->  (maps-to)
  ("->\\*", "\\rightarrow^*"),       -- ->*  (small-step evaluation, multi)
  ("->", "\\rightarrow"),            -- ->   (small-step evaluation)
  ("<-", "\\leftarrow"),             -- <-   (assignment)
  ("\\|=", "\\models"),              -- |=   (double turnstile)
  ("\\|-", "\\vdash"),               -- |-   (turnstile)
  ("<=", "\\leq"),                   -- <=   (less than or equal to)
  (">=", "\\geq"),                   -- >=   (greater than or equal to)
  ("<<", "\\ll"),                    -- <<   (far less than)
  (">>", "\\gg"),                    -- >>   (far greater than)
  ("!=", "\\neq"),                   -- !=   (not equal to)
  ("==", "\\equiv"),                 -- ==   (equivalence)
  ("\\.\\.\\.\\.\\.\\.", "\\cdots"), -- ......
  ("\\.\\.\\.", "\\dots"),           -- ...
  ("~", "\\sim"),                    -- ~
  ("[[:blank:]]\\*[[:blank:]]", "\\times "),       -- *    (multiplication)
  ("[[:blank:]]\\.[[:blank:]]", "\\,.\\,"),        -- .    (dot with margins)
  ("[[:blank:]|\n]&[[:blank:]|\n]", "\\qquad "),   -- &    (extra spacing)
  ("[[:blank:]|\n]\\|[[:blank:]|\n]", "\\,|\\,"),  -- |    (bar with margins)
  ("!", "\\downarrow") ]             -- !    (big-step evaluation)

-- grouped braces ( "<[" "]>" )
subLBrace s =
  subRegex (mkRegex "<\\[") s $ "\\left\\{"
subRBrace s =
  subRegex (mkRegex "\\]>") s $ "\\right\\}"

-- box
subLBox s =
  subRegex (mkRegex "\\|\\[") s $ "\\boxed{"
subRBox s =
  subRegex (mkRegex "\\]\\|") s $ "}"

-- underline
subLUnderline s =
  subRegex (mkRegex "_\\[") s $ "\\underline{"
subRUnderline s =
  subRegex (mkRegex "\\]_") s $ "}"

-- overline
subLOverline s =
  subRegex (mkRegex "-\\[") s $ "\\overline{"
subROverline s =
  subRegex (mkRegex "\\]-") s $ "}"

-- floor
subLFloor s =
  subRegex (mkRegex "\\[_") s $ "\\lfloor "
subRFloor s =
  subRegex (mkRegex "_\\]") s $ "\\rfloor "

-- angle brackets (no internal spacing, distinguished from less/greater-than)
subLAngle s =
  subRegex (mkRegex "<([^[:blank:]])") s $ "\\langle \\1"
subRAngle s =
  subRegex (mkRegex "([^[:blank:]])>") s $ "\\1\\rangle "

-- grouped double square brackets
subLLBracket s =
  subRegex (mkRegex "\\[\\[") s $ "[\\!["
subRRBracket s =
  subRegex (mkRegex "\\]\\]") s $ "]\\!]"

-- grouped square brackets
subLBracket s =
  subRegex (mkRegex "\\[") s $ "\\left["
subRBracket s =
  subRegex (mkRegex "\\]") s $ "\\right]"

-- grouped parentheses
subLParen s =
  subRegex (mkRegex "\\(") s $ "\\left("
subRParen s =
  subRegex (mkRegex "\\)") s $ "\\right)"


-- main filter
semType :: Inline -> IO Inline
semType m@(Math mathType s) = do
  let s' =
        subLParen $ subRParen $
        subLBracket $ subRBracket $
        subLLBracket $ subRRBracket $
        subLAngle $ subRAngle $
        subLFloor $ subRFloor $
        subLOverline $ subROverline $
        subLUnderline $ subRUnderline $
        subLBox $ subRBox $
        subLBrace $ subRBrace $
        subUniqueNotation $
        subKeywordText $
        subSpecialSymbol $
        subDerivation $
        sub rule $
        sub deriveFrom $
        sub namedSubDerivation $
        sub namedSubDerivationLast s
  --d s'
  return $ Math mathType s'
semType x = return x

main :: IO ()
main = toJSONFilter semType
