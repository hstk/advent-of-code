module Mass where

import Data.Function ((&))

main :: IO ()
main = do
  masses <- input
  let moduleFuelReqs = requiredFuel <$> masses
  let recursiveFuelReqs = fuelForFuel <$> moduleFuelReqs

  putStrLn "Fuel for modules:"
  print $ sum moduleFuelReqs
  putStrLn "Total fuel:"
  print $ (sum moduleFuelReqs) + (sum recursiveFuelReqs)

fuelForFuel :: Int -> Int
fuelForFuel fuel = let f = requiredFuel fuel in
  case f > 0 of
    True  -> f + fuelForFuel f
    False -> 0

requiredFuel :: Int -> Int
requiredFuel mass = subtract 2 $ div mass 3

input :: IO [Int]
input = do 
  text <- words <$> readFile inputPath
  let ints = read <$> text
  pure ints

inputPath :: FilePath
inputPath = "2019\\day1\\input.txt"