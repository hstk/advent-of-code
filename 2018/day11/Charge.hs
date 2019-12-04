module Charge where

import           Control.Applicative
import           Control.Parallel
import           Control.Parallel.Strategies
import qualified Data.Sequence as S
import           Data.Sequence (Seq(..))
import           Data.Foldable
import           Data.Function
import           Data.Vector (Vector(..))
import qualified Data.Vector as V
import           Text.Trifecta

-- type Grid = [Cell]
data Cell = Cell
  { xPos :: Int
  , yPos :: Int
  , power :: Int
  } deriving (Eq, Show)

main :: IO ()
main = do


  putStrLn "wut"

-- | Get cells to bottom and right in 3x3 grid
getFrame :: Cell -> [Cell] -> [Cell]
getFrame x xs = do
  undefined

serial :: Int
serial = 9995



-- grid :: Grid
grid' = fmap (rows) [1..300]
  where rows y = do
          x <- [1..300]
          let power = findPower x y
          pure $ Cell x y power

findPower :: Int -> Int -> Int
findPower x y =
  let rackId = x + 10
      powerLevel = rackId * y
      addingSerial = powerLevel + serial
      setTo = addingSerial * rackId
      hundo = findHundredsDigit setTo  
  in hundo - 5

findHundredsDigit n
  | n >= 1000 = findHundredsDigit (mod n 1000)
  | n >= 100  = div n 100
  | otherwise = 0
