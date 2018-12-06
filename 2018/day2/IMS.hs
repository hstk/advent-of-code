module IMS where

import Common
import Text.Trifecta
import Data.Function
import Data.List
import Data.Maybe

type Inventory = String

main :: IO ()
main = do
  maybeInv <- parseFromFile (parseInventory) "2018\\day2\\input.txt"
  let inv = fromMaybe [] maybeInv

  let checksum1 = (countWith (hasNReps 2) inv) * (countWith (hasNReps 3) inv)
  putStrLn $ "Stage 1 checksum: " <> show checksum1

  let stage2 = case setCompare inv of
                Just (a, b) -> zip a b 
                             & filter (\x -> fst x == snd x) 
                             & unzip 
                             & fst
                Nothing -> "no dice"

  putStrLn $ "Stage 2 intersection: " <> show stage2

countWith :: (a -> Bool) -> [a] -> Int
countWith pred xs = length $ filter pred xs

hasNReps :: Ord a => Int -> [a] -> Bool
hasNReps n = any (\x -> length x == n) . group . sort

setCompare :: [String] -> Maybe (String, String)
setCompare (x:xs) = case find (onePermutation x) xs of
  Just match -> Just (x, match)
  Nothing    -> setCompare xs
setCompare _ = Nothing

onePermutation :: Eq a => [a] -> [a] -> Bool
onePermutation a b = allButOne id $ zipWith (==) a b

allButOne :: (a -> Bool) -> [a] -> Bool
allButOne pred xs = (length xs - 1) == (countWith pred xs)

parseInventory :: Parser [Inventory]
parseInventory = linesOf (many (notChar '\n'))

