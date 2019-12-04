module Charts where

import qualified Data.Sequence as S
import           Data.Sequence (Seq)

data Pos = Pos { a :: Int, b :: Int }
type Recipe = Int
type Chart = Seq Recipe

combine :: Recipe -> Recipe -> Chart
combine a b = S.fromList $ foldr (\x acc -> (read [x] :: Int) : acc) [] (show $ a + b)

combineAndAppend :: Chart -> Recipe -> Recipe -> Chart
combineAndAppend chart a b = chart <> (combine a b)

-- fix this garbage
findNewPositions :: Chart -> Int -> Int
findNewPositions chart ix = 
  if advanceToIx <= (S.length chart - 1)
    then advanceToIx
    else undefined
  where advanceToIx = (ix + (S.index chart ix) + 1)
        len = S.length chart

advance :: Int -> Chart -> Chart
advance n c = go n c (Pos 0 1) where
  go 0 c _ = c
  go n c (Pos a b) = go (n - 1) newChart newIndices where
    (aRec, bRec) = (S.index c a, S.index c b)
    newChart = combineAndAppend c aRec bRec
    newIndices = Pos (findNewPositions newChart a) 
                     (findNewPositions newChart b)

main :: IO ()
main = do
  let chart = S.fromList $ [3, 7]
  print $ advance 5 chart

inp :: Int
inp = 327901

cerp = S.fromList [3,7,1,0]
zerp = findNewPositions cerp
