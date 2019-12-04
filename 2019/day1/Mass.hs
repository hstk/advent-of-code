module Mass where

import Data.Function ((&))

main :: IO ()
main = do
  masses <- input
  print . sum $ requiredFuel <$> masses

requiredFuel :: Int -> Int
requiredFuel mass = mass & div 3 & (-) 2

input :: IO [Int]
input = do 
  text <- words <$> readFile inputPath
  let ints = read <$> text
  pure ints

inputPath :: FilePath
inputPath = "2019\\day1\\input.txt"