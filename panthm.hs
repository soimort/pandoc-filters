#!/usr/bin/env runhaskell
{-
  panthm.hs
  A Pandoc filter that makes theorems great!

  Dependencies:
  - regex-compat
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

listToStr :: [Inline] -> String
listToStr (Str s : xs) = s ++ listToStr xs
listToStr (Space : xs) = " " ++ listToStr xs
listToStr [] = ""

-- main filter
panTheorem :: Block -> IO Block
panTheorem b@(Para p@(x:xs)) = do
  case x of
    Strong x'@(y@(Str z) : ys) ->
      if elem z [ "Definition", "Condition", "Problem",  -- definition
                  "Example", "Exercise", "Algorithm", "Question",
                  "Axiom", "Property", "Assumption", "Hypothesis" ]
      then
        let y' = RawInline (Format "tex") ("\\textsc{" ++ z ++ "}")
        in
        return $ Para ([ RawInline (Format "tex") "\\noindent",
                         SoftBreak ] ++ Strong (y' : ys) : xs)
      else if elem z [ "Theorem", "Lemma", "Corollary", -- plain
                       "Proposition", "Conjecture", "Criterion", "Assertion" ]
           then
             let y' = RawInline (Format "tex") ("\\textsc{" ++ z ++ "}")
             in
             return $ Para ([ RawInline (Format "tex") "\\noindent",
                              SoftBreak ] ++ Strong (y' : ys) : xs)
           else if elem z [ "Remark", "Note", "Notation", "Claim", -- remark
                            "Summary", "Acknowledgment", "Case", "Conclusion" ]
                then
                  let y' = RawInline (Format "tex") ("\\textsc{" ++ z ++ "}")
                  in
                  return $ Para ([ RawInline (Format "tex") "\\noindent",
                                   SoftBreak ] ++ Strong (y' : ys) : xs)
                else
                  case last x' of
                    Str w ->
                      if last w == '.'
                      then
                        let x'' = RawInline (Format "tex") ("\\textsc{" ++ listToStr x' ++ "}")
                        in
                        return $ Para ([ RawInline (Format "tex") "\\noindent",
                                         SoftBreak ] ++ Strong [ x'' ] : xs)
                      else
                        return b
                    _ -> return b
    Str s ->
      if s == "Proof."
      then
        return $ Para ([ RawInline (Format "tex") "\\noindent",
                         SoftBreak ] ++ Emph [Str s] : xs)
      else
        return b
    _ -> return b
panTheorem b = return b

main :: IO ()
main = toJSONFilter panTheorem
