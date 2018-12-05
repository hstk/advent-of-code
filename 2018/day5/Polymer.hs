module Polymer where

import           Common
import           Control.Applicative
import           Data.Function ((&))
import           Data.List
import           Data.Char
import           Text.Trifecta

type Polymer = String

main :: IO ()
main = do
  polymer <- parseInput (whiteSpace >> many letter) [] "2018\\day5\\input.txt"
  let reacted = reducePolymer polymer
  putStrLn $ "Length after reduction: " <> (show $ length reacted)
  putStrLn $ "Lowest after removing an element: " <> (show $ findMinVariation reacted)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint endo a = if endo a == a then a else endo a

isReduciblePair :: Char -> Char -> Bool
isReduciblePair a b = case (isUpper a, isUpper b) of
  (True, True)   -> False
  (False, False) -> False
  _              -> toUpper a == toUpper b

reducePolymer :: Polymer -> Polymer
reducePolymer = fixpoint $ foldr collapse [] where
  collapse x [] = x:[]
  collapse x (a:as) =
    if isReduciblePair x a
    then as
    else x:a:as

removeUnitAndFoldToLength :: Polymer -> Char -> Int
removeUnitAndFoldToLength poly ch = poly
  & filter (\x -> x /= ch && x /= (toUpper ch))
  & reducePolymer
  & length

findMinVariation :: Polymer -> Int
findMinVariation xs = minimum $ fmap (removeUnitAndFoldToLength xs) ['a'..'z']