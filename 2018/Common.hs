module Common where

import Text.Trifecta
import Data.Maybe

fromResult :: a -> Result a -> a
fromResult base res = case res of
  Success a -> a
  _ -> base

linesOf :: Parser a -> Parser [a]
linesOf p = sepBy1 p newline <* eof

tp :: Parser a -> String -> Result a
tp p = parseString p mempty

parseInput :: Parser a -> a -> FilePath -> IO a 
parseInput parser base path = do 
  m <- parseFromFile parser path
  pure $ fromMaybe base m

int :: Parser Int
int = fmap fromIntegral integer

countWith :: (a -> Bool) -> [a] -> Int
countWith pred xs = length $ filter pred xs