module IMS where

import Common
import Text.Trifecta
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map(..))

type Inventory = String

main :: IO ()
main = do
  maybeInv <- parseFromFile (parseInventory) inputPath
  let inv = fromMaybe [] maybeInv

  let checksum1 = (countPred (hasNReps 2) inv) * (countPred (hasNReps 3) inv)
  putStrLn $ "Stage 1 checksum: " <> show checksum1

  let stage2 = case setCompare inv of
                Just (a, b) -> zip a b & filter (\x -> fst x == snd x) & unzip & fst
                Nothing -> "no dice"

  putStrLn $ "Stage 2 intersection: " <> show stage2

setCompare :: [String] -> Maybe (String, String)
setCompare (x:xs) = case find (onePermutation x) xs of
  Just match -> Just (x, match)
  Nothing    -> setCompare xs
setCompare _ = Nothing

countPred :: (a -> Bool) -> [a] -> Int
countPred pred xs = length $ filter pred xs

parseInventory :: Parser [Inventory]
parseInventory = linesOf (many (notChar '\n'))

hasNReps :: Ord a => Int -> [a] -> Bool
hasNReps n = any (\x -> length x == n) . group . sort

onePermutation :: (Eq a) => [a] -> [a] -> Bool
onePermutation a b = allButOne id $ zipWith (==) a b

allButOne :: (a -> Bool) -> [a] -> Bool
allButOne pred xs = (length xs - 1) == (countPred pred xs)

inputPath :: FilePath
inputPath = "2018\\day2\\input.txt"
