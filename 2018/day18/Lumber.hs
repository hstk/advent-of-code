{-# LANGUAGE QuasiQuotes #-}

module Lumber where

import           Common
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Array.IArray
import           Data.Foldable (any)
import           Data.Maybe
import           Text.RawString.QQ
import           Text.Trifecta

data Tile = Open | Trees | Lumberyard
  deriving (Eq, Show)
type Pos = (Int, Int)
type Grid = Array Pos Tile

-- figured this might come with array, but no
access :: Grid -> Pos -> Maybe Tile
access arr ix@(y, x) = 
  if y < minY || x < minX || y > maxY || x > maxX
  then Nothing 
  else Just $ arr ! ix
  where ((minY, minX), (maxY, maxX)) = bounds arr

parseTile :: Parser Tile
parseTile = choice [open, tree, lumb]
  where open = char '.' *> pure Open
        tree = char '|' *> pure Trees
        lumb = char '#' *> pure Lumberyard

updateTile :: Pos -> Grid -> Tile
updateTile p g = case access g p of
  Just Open       -> if hasThree Trees
                     then Trees else Open
  Just Trees      -> if hasThree Lumberyard
                     then Lumberyard else Trees
  Just Lumberyard -> if remainsLumber
                     then Lumberyard else Open
  _               -> error $ "bad index called on " <> (show p)
  where surroundings = catMaybes $ (access g) <$> (relativeCoords p)
        hasThree x = (countWith ((==) x) surroundings) >= 3
        remainsLumber = any ((==) Lumberyard) surroundings &&
                        any ((==) Trees) surroundings

relativeCoords :: Pos -> [Pos]
relativeCoords p@(x, y) = do
  x' <- [x - 1, x, x + 1]
  y' <- [y - 1, y, y + 1]
  guard $ (x', y') /= p
  pure (x', y')

parseGrid :: Parser [[Tile]]
parseGrid = manyTill (many parseTile <* whiteSpace) eof

rowsToArray :: [[a]] -> Array Pos a
rowsToArray xs = do
  let height = length xs
  let width = length $ head xs
  listArray ((0,0),(width - 1, height - 1)) (join xs)

main = do 
  rows <- parseInput parseGrid [] "2018\\day18\\input.txt"
  let grid = listArray ((0,0),(49,49)) (join rows) :: Grid
  let res = runGrid 10 grid
  let woods = foldr (\x acc -> if (x == Trees) then acc + 1 else acc) 0 res
  let yards = foldr (\x acc -> if (x == Lumberyard) then acc + 1 else acc) 0 res

  putStrLn $ "Woods * lumberyards: " <> (show $ woods * yards)
  -- let after10 = 

  -- gotta run 1000000000 cycles; millenia if those are minutes
  -- willing to bet this cycles; need to figure out some kind of cycle detection here
  let res' = runGrid 1000000000 grid
  let woods' = foldr (\x acc -> if (x == Trees) then acc + 1 else acc) 0 res'
  let yards' = foldr (\x acc -> if (x == Lumberyard) then acc + 1 else acc) 0 res'
  putStrLn $ "if stack hasn't blown, resource value is :" <> (show $ woods' * yards')

updateGrid :: Grid -> Grid
updateGrid g = array (bounds g) ((\x -> (x, updateTile x g)) <$> indices g)

runGrid :: Int -> Grid -> Grid
runGrid times grid = 
  execState (replicateM times $ state $ \x -> ((), updateGrid x)) grid

raw = rowsToArray $ fromResult [] $ tp parseGrid raw'
raw' = drop 1 $ [r|
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
|]

gold = rowsToArray $ fromResult [] $ tp parseGrid gold'
gold' = drop 1 $ [r|
.||##.....
||###.....
||##......
|##.....##
|##.....##
|##....##|
||##.####|
||#####|||
||||#|||||
||||||||||
|]
