module ChronalCalibration where

import Common
import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set(..))
import Text.Trifecta

main = do
  parsedIntList <- parseFromFile (many (whiteSpace >> integer)) inputPath
  let intList = fromMaybe [] parsedIntList
  let summed = foldr (+) 0 intList
  putStrLn $ "Sum: " <> show summed

  let cyclicalSummed = scanl (+) 0 (cycle intList)
  let firstRep = foldM failOnDuplicate (S.empty) cyclicalSummed
  putStrLn $ "First repeated result: " <> show firstRep

failOnDuplicate :: Ord a => Set a -> a -> Either a (Set a)
failOnDuplicate set a = 
  if (S.member a set) 
  then Left a 
  else Right $ S.insert a set

inputPath :: FilePath
inputPath = "2018\\day1\\input.txt"
