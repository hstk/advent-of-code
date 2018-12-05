module Polymer where

import           Common
import           Control.Applicative
import           Data.List
import           Data.Char
import           Text.Trifecta

type Polymer = String

main :: IO ()
main = do
  polymer <- parseInput (whiteSpace >> many letter) [] inputPath
  putStrLn $ "Length before reduction: " <> (show $ length polymer)
  let fullyReduced = fix foldPolymer polymer
  putStrLn $ "Length after reduction: " <> (show $ length fullyReduced)
  let withoutUnit = fmap (length . fix foldPolymer) (variations fullyReduced)
  putStrLn $ "Shortest after removing one unit type: " <> (show $ minimum withoutUnit)

fix :: Eq a => (a -> a) -> a -> a
fix endo a = if endo a == a then a else endo a

isReduciblePair :: Char -> Char -> Bool
isReduciblePair a b = case (isUpper a, isUpper b) of
  (True, True)   -> False
  (False, False) -> False
  _              -> toUpper a == toUpper b

foldPolymer :: Polymer -> Polymer
foldPolymer = foldr collapse [] where 
  collapse x [] = x:[]
  collapse x (a:as) =
    if isReduciblePair x a
    then as
    else x:a:as

variations :: Polymer -> [Polymer]
variations xs = do
  let removePair (x1, x2) = filter (\a -> a /= x1 && a /= x2)
  let range = zip ['a'..'z'] ['A'..'Z']
  removePair <$> range <*> pure xs

inputPath :: FilePath
inputPath = "2018\\day5\\input.txt"
