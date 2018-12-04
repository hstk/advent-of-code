module Challenge where

import           Common
import           Control.Applicative
import           Data.Function ((&))
import           Data.Functor ((<&>))
-- import qualified Data.IntMap as IM
-- import           Data.IntMap (IntMap(..))
import           Data.List (sort)
import           Data.Time
import           Text.Trifecta

main :: IO ()
main = do
  logs <- parseInput (many (parseLog <* whiteSpace)) [] inputPath
  print $ take 10 logs
  print $ sort $ take 10 logs

  undefined

type Guard = Int
-- type Time = Int --00:00 -> 0, 23:59 -> 1439
type TimeSpan = (Int, Int)
data Event = Sleep | Awake | Begin Guard | End Guard
  deriving (Eq, Show)
data LogEntry = LogEntry Event LocalTime
  deriving (Eq, Show)

instance Ord LogEntry where
  compare (LogEntry _ a) (LogEntry _ b) = compare a b 

data Shift = Shift
  { guardId :: Guard
  , start :: LocalTime
  , events :: [LogEntry]
  , end :: LocalTime
  } deriving (Eq, Show)

parseLog :: Parser LogEntry
parseLog = do
  whiteSpace
  time <- parseTimestamp
  whiteSpace
  event <- parseEvent
  pure $ LogEntry event time

parseTimestamp :: Parser LocalTime
parseTimestamp = do
  char '['
  year <- integer
  char '-'
  month <- int
  char '-'
  day <- int
  hour <- int
  colon
  minutes <- int
  char ']'
  pure $ LocalTime 
        (fromGregorian year month day) 
        (TimeOfDay hour minutes 0)

-- [1518-06-18 00:00] Guard #1499 begins shift
-- [1518-11-21 00:48] wakes up
-- [1518-05-19 00:29] falls asleep
-- [1518-03-20 23:58] Guard #73 begins shift

wake  = string "wakes up" *> pure Awake
sleep = string "falls asleep" *> pure Sleep
beginShift = Begin <$> (string "Guard #" *> int <* string "begins shift")

parseEvent :: Parser Event
parseEvent = choice [wake, sleep, beginShift]

inputPath :: FilePath
inputPath = "2018\\day4\\input.txt"
