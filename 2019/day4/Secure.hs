{-# LANGUAGE NumericUnderscores #-}

module Secure where

import Data.Char (digitToInt)
import Data.Foldable (foldMap)
import Data.Monoid (Any(..), All(..))
import Data.List (group)

main :: IO ()
main = do
  let validPasses = filter validPassword1 input
  let validPasses2 = filter hasIsolatedPairSubseq validPasses

  putStrLn "Number of valid passes in input (stage 1): "
  print $ length validPasses
  putStrLn "Number of valid passes in input (stage 2): "
  print $ length validPasses2

input :: [Int]
input = [134792..675810]

isSixDigit :: Int -> Bool
isSixDigit n = n >= 100_000 && n < 1_000_000

-- offsetZipAsIntListWith f 1223
-- zipWith f [1,2,2,5] [2,2,5]
-- [f 1 2, f 2 2, f 2 5]
offsetZipAsIntListWith :: (Int -> Int -> b) -> Int -> [b]
offsetZipAsIntListWith f xs =
  let asInts = fmap digitToInt . show $ xs
  in 
    zipWith f asInts (drop 1 asInts)

-- offsetZipAsIntListWith (<=) [1,2,5,5,8]
-- [1 <= 2, 2 <= 5, 5 <= 5, 5 <= 8]
hasIncreasingIntSeq :: Int -> Bool
hasIncreasingIntSeq n = 
  let increasing = offsetZipAsIntListWith (<=) n
  in 
    getAll $ foldMap All increasing

-- offsetZipAsIntListWith (==) [1,2,2,3]
-- [1 == 2, 2 == 2, 2 == 3]
hasPairSubseq :: Int -> Bool
hasPairSubseq n =
  let isPair = offsetZipAsIntListWith (==) n
  in 
    getAny $ foldMap Any isPair

-- for this condition, having a pair that isn't part of a larger group
-- we can look for an isolated pair using the group function
-- the presence of a single element True anywhere is adequate
-- offsetZipAsIntListWith (==) [1,1,2,2,2,2]
-- [True, False, True, True, True]
-- group it
-- [[True], [False], [True, True, True]]
hasIsolatedPairSubseq :: Int -> Bool
hasIsolatedPairSubseq n = 
  let groupedPairs = group $ offsetZipAsIntListWith (==) n
  in 
    [True] `elem` groupedPairs

validPassword1 :: Int -> Bool
validPassword1 n = isSixDigit n 
  && hasIncreasingIntSeq n
  && hasPairSubseq n
