module Grid where

import           Common
import           Control.Applicative
import           Control.Parallel
import           Control.Parallel.Strategies
import           Control.Monad
import           Debug.Trace
import           Data.Function
import           Data.Functor ((<&>))
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map(..))
import qualified Data.Sequence as S
import           Data.Sequence (Seq(..))
import           Data.Maybe
import           Text.Trifecta

main :: IO ()
main = do
  locs <- sort <$> parseInput (many parseLoc) [] "2018\\day6\\input.txt"
  putStrLn $ "Length of initial list: " <> (show $ length locs)
  let searchList = catMaybes $ fmap (determineFiniteness locs) locs
  putStrLn $ "Length of reduced list: " <>  (show $ length searchList)

type Point       = (Int, Int)
data Target   = Target { loc :: Point, area :: Int }
  deriving (Eq, Show)
type Distance    = Int
data Claim       = Claim Point Distance
data Ownership   = OwnedBy Point | Contested | OccupiedBy Point | Unclaimed
  deriving (Eq, Show)



data Region = Region
  { up     :: Point
  , down   :: Point
  , left   :: Point
  , right  :: Point
  , origin :: Point
  } deriving (Eq, Show)

boundedBoxFromRegion :: Region -> [Point]
boundedBoxFromRegion r = do
  -- x <- [(fst $ left r) + 1 .. (fst $ right r) - 1]
  -- y <- [(snd $ up r) + 1 .. (snd $ down r) - 1]
  x <- [(fst $ left r) .. (fst $ right r)]
  y <- [(snd $ up r) .. (snd $ down r)]
  pure (x, y)

pointToSearchRegion ::  [Point] -> Point -> Region
pointToSearchRegion pts origin = do
  -- let xSorted = sortOn (xDist origin) pts
  -- let ySorted = sortOn (yDist origin) pts
  let mhSorted = sortOn (mhDist origin) pts
  let up =    find (\x -> x `isAbove` origin) mhSorted
  let down =  find (\x -> x `isBelow` origin) mhSorted
  let left =  find (\x -> x `isLeftOf` origin) mhSorted
  let right = find (\x -> x `isRightOf` origin) mhSorted

  Region { up = fromJust up
         , down = fromJust down
         , left = fromJust left
         , right = fromJust right
         , origin = origin }

-- ensure there are other points on all sides
-- equivalent to finding the max / min x / y and ensuring point is inside
determineFiniteness :: [Point] -> Point -> Maybe Point
determineFiniteness pts target = do
  ul <- find (\x -> x `isLeftOf`  target && x `isAbove` target) pts
  ur <- find (\x -> x `isRightOf` target && x `isAbove` target) pts
  ll <- find (\x -> x `isLeftOf`  target && x `isBelow` target) pts
  lr <- find (\x -> x `isRightOf` target && x `isBelow` target) pts
  -- there might be an edge case, we might only need to triangulate
  -- also might not be the best finiteness test
  -- beware
  pure target

-- boring utility funcs
mhDist :: Point -> Point -> Int
mhDist (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

xDist :: Point -> Point -> Int
xDist a b = abs $ fst a - fst b 
yDist :: Point -> Point -> Int
yDist a b = abs $ snd a - snd b

compareX :: Point -> Point -> Ordering
compareX = compare `on` fst
compareY :: Point -> Point -> Ordering
compareY = compare `on` snd

isLeftOf :: Point -> Point -> Bool
isLeftOf = (<) `on` fst
isRightOf :: Point -> Point -> Bool
isRightOf = (>) `on` fst
isAbove :: Point -> Point -> Bool
isAbove = (<) `on` snd
isBelow :: Point -> Point -> Bool
isBelow = (>) `on` snd

testAreas :: [Point]
testAreas = [
  (1, 1),
  (1, 6),
  (8, 3),
  (3, 4),
  (5, 5),
  (8, 9)]

parseLoc :: Parser Point
parseLoc = do
  x <- int
  char ','
  whiteSpace
  y <- int
  pure (x, y)

testLocs = parseInput (many parseLoc) [] "2018\\day6\\input.txt"
