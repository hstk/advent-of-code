{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Align where

import           Prelude hiding (reverse, minimum, maximum)
import           Control.Monad.ST
import           Common
import           Data.Foldable
import           Text.RawString.QQ
import           Text.Trifecta

data Star = Star Pos Vel
  deriving Show

data Pos = Pos Int Int deriving (Eq, Ord)

instance Show Pos where
  show (Pos a b) = "<" <> (show a) <> ", " <> (show b) <> ">" 

data Vel = Vel (Int -> Int) (Int -> Int) -- maybe cleaner as (Pos -> Pos)
instance Show Vel where
  show (Vel a b) = "<" <> (show $ a 0) <> ", " <> (show $ b 0) <> ">"

data Global = Global
  { stars :: [Star]
  -- , starfield :: Array (Array Star)
  , iterations :: Int
  } deriving Show

main :: IO ()
main = do
  starfield <- p
  putStrLn $ "Stars in the sky: " <> (show $ length starfield)
  let initial = Global starfield 0
  putStrLn $ "Total area: " <> (show $ totalArea starfield)
  -- 10000 seems fairly close
  let (r, zerp) = runGlobal initial 10000
  putStrLn $ "After 10000 iterations: " <> show r
  putStrLn "haha wut"

forward :: Star -> Star
forward (Star (Pos x y) v@(Vel fx fy)) = Star (Pos (fx x) (fy y)) v

reverse :: Star -> Star
reverse (Star (Pos x y) v@(Vel fx fy)) = Star (Pos (x + rx) (y + ry)) v
  where rx = negate $ fx 0
        ry = negate $ fy 0

cycleStar :: Star -> Int -> Star
cycleStar s 0 = s -- partial, fix
cycleStar s n = cycleStar (forward s) (n - 1)

runGlobal :: Global -> Int -> (Int, Global)
runGlobal g n = 
  let advancedStars = fmap (\x -> cycleStar x n) $ stars g 
      area = totalArea advancedStars
  in (area, Global advancedStars n)

-- | Get total area between the 4 stars with highest / lowest x / y vals.
totalArea :: (Foldable f, Functor f) => f Star -> Int
totalArea xs = let
  positions = fmap (\(Star p _) -> p) xs
  (Pos x1 y1) = minimum positions
  (Pos x2 y2) = maximum positions
  in (x2 - x1) + (y2 - y1)

-- | LRUD
getWindow :: (Foldable f, Functor f) => f Star -> (Int, Int, Int, Int)
getWindow xs = let
  positions = fmap (\(Star p _) -> p) xs
  (Pos x1 y1) = minimum positions
  (Pos x2 y2) = maximum positions
  in (x1, x2, y2, y1) 

translateStarfield :: Global -> Global
translateStarfield (Global field acc) = Global newField acc
  where leastStar@(Star (Pos leastX leastY) _) = minimum field
        trans (Star (Pos x y) vel) = Star (Pos (x - leastX) (y - leastY)) vel
        newField = fmap trans field

parseStar :: Parser Star
parseStar = do
  let int' = optional whiteSpace >> int
  optional whiteSpace
  string "position=<"
  xPos <- int'
  char ','
  yPos <- int'
  string "> velocity=<"
  xVel <- int'
  char ','
  yVel <- int'
  char '>'
  pure $ Star (Pos xPos yPos) (Vel ((+) xVel) ((+) yVel))

exInput :: String
exInput = [r|position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>|]
