module Common where

import Text.Trifecta

fromResult :: a -> Result a -> a
fromResult base res = case res of
  Success a -> a
  _ -> base

linesOf :: Parser a -> Parser [a]
linesOf p = sepBy1 p newline <* eof