module Challenge where

import           Common
import           Control.Applicative
import           Data.Function
import           Data.Functor ((<&>))
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map(..))
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap(..))
import           Data.Maybe
import           Text.Trifecta

main :: IO ()
main = do
  locs <- parseInput (many parseLoc) [] "2018\\day6\\input.txt"
  -- let cartesian = 
  print $ locs

type Location = (Int, Int)

testLocs = parseInput (many parseLoc) [] "2018\\day6\\input.txt"

parseLoc :: Parser Location
parseLoc = do
  x <- int
  char ','
  whiteSpace
  y <- int
  pure (x, y)

testAreas :: [Location]
testAreas = [
  (1, 1),
  (1, 6),
  (8, 3),
  (3, 4),
  (5, 5),
  (8, 9)]

mhDist :: Location -> Location -> Int
mhDist (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

xDist :: Location -> Location -> Int
xDist a b = abs $ fst a - fst b 

yDist :: Location -> Location -> Int
yDist a b = abs $ snd a - snd b 

compareX :: Location -> Location -> Ordering
compareX = compare `on` fst

compareY :: Location -> Location -> Ordering
compareY = compare `on` snd

ul :: Location -> [Location] -> Maybe Location
ul target locs = do
  

--finds nearest 4 other points
-- findNeighbors :: Location -> [Location] -> Maybe [Location]
findNeighbors target locs = do
  let ul = filter (\x -> compareY target x)


  let xAxis = take 2 $ sortOn (xDist target) locs
  let yAxis = take 2 $ sortOn (yDist target) locs
  xAxis <> yAxis

fnMh target locs = take 4 $ sortOn (mhDist target) locs

-- findClosestPoint :: Location -> [Location] -> Location
findClosestPoint target locs = sortOn fst $ fmap (\x -> (mhDist target x, x)) locs
-- 
{-
For each point: if there is no neighor to the top bottom left right, disqualify.



Find the mh distance between nearest neighbors
midpoints are discarded in this comparsion, (1,1) and (1,5) each have two noninfinite points 


-}