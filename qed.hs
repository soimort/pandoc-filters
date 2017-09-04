#!/usr/bin/env runhaskell
{-
  Pandoc filter to convert raw LaTeX "\Qed" or end-of-paragraph string "[QED]"
  into tombstone symbol. (HTML only)
-}
import Text.Pandoc.JSON
import System.IO

d :: Show a => a -> IO ()
d = hPutStrLn stderr . show

fmt :: Format
fmt = Format "html"

htmlTombstone :: String
htmlTombstone =
  "<p style='text-align:right !important;" ++
  "text-indent:0 !important;" ++
  "position:relative;top:-1em'>&#9632;</p>"

qed :: Block -> IO Block
qed b@(RawBlock (Format "latex") "\\Qed") =
  return $ RawBlock fmt htmlTombstone
qed b@(Para ins) | last ins `elem` [ Str "[QED]",
                                     -- "tex" rather than "latex" -- why?
                                     RawInline (Format "tex") "\\Qed" ] =
                     return $ Para $ init ins ++ [
                     RawInline fmt htmlTombstone ]
                 | otherwise = return b
qed x = return x

main :: IO ()
main = toJSONFilter qed
