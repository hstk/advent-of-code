module Repose where

import           Common
import           Control.Applicative
import           Data.Function
import           Data.Functor ((<&>))
import           Data.List (sort,sortBy, group, groupBy)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map(..))
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap(..))
import           Data.Maybe
import           Data.Time
import           Data.Time.LocalTime
import           Data.Time.Calendar
import           Text.Trifecta

main :: IO ()
main = do
  logs <- sort <$> parseInput (many (parseLog <* whiteSpace)) [] inputPath
  let logsByShift = groupByShift logs
  let shifts = processShift <$> logsByShift
  let sleepiestGuard = head $ reverse $ sortBy (compare `on` snd) $ M.toList $ sumSleepByGuard shifts
  putStrLn $ "Sleepiest guard: " <> (show sleepiestGuard)
  let minuteMap = shiftsToMinuteMap shifts
  let minutes = IM.lookup (fst sleepiestGuard) minuteMap
  putStrLn $ "Minute most likely to be sleeping by sleepiest: " <>
    (show . findMostFrequentMinute $ fromJust minutes) -- I know, I know
  let summarizedMap = fmap findMostFrequentMinute minuteMap
  let predictableGuard = head $ reverse $ sortBy (compare `on` snd) $ IM.toList $ summarizedMap
  putStrLn $ "Most common minute slept: " <> (show predictableGuard)

findMostFrequentMinute :: [Minute] -> (Int, Minute)
findMostFrequentMinute = head . reverse . sort . fmap (\x -> (length x, head x)) . group . sort

data LogEntry = LogEntry Event LocalTime
  deriving (Eq)

instance Show LogEntry where
  show (LogEntry event (LocalTime day (hms))) = 
    "[" <> show day <> " " <> show hms <> "]" <> " " <> show event

instance Ord LogEntry where
  compare (LogEntry _ a) (LogEntry _ b) = compare a b

data Shift = Shift
  { guardId :: Guard
  , start :: LocalTime
  , events :: [LogEntry]
  , sleepPeriods :: [Int]
  , totalSleep :: Int
  } deriving (Eq, Show)

type Guard = Int
type Minute = Int
data Event = FallAsleep | WakeUp | Begin Guard
  deriving (Eq, Show)

shiftsToMinuteMap :: [Shift] -> IntMap [Minute]
shiftsToMinuteMap xs = do 
  let entries =  xs >>= sleepPeriodToEntries
  IM.fromListWith (<>) entries

sleepPeriodToEntries :: Shift -> [(Guard, [Minute])]
sleepPeriodToEntries shift = fmap (\x -> (guardId shift, [x])) (sleepPeriods shift)

groupByShift :: [LogEntry] -> [[LogEntry]]
groupByShift = groupBy sameShift

-- precondition: sorted input
sameShift :: LogEntry -> LogEntry -> Bool
sameShift a (LogEntry b _) = case b of
  Begin _ -> False
  _       -> True

processShift :: [LogEntry] -> Shift
processShift ((LogEntry (Begin guard) startTime):events) =
  let sleepTime = eventsToSleepRanges events in
  Shift { guardId = guard
        , start = startTime
        , events = events
        , sleepPeriods = sleepTime
        , totalSleep = length sleepTime }
processShift _ = error "first event isn't a shift start, bad grouping"

sumSleepByGuard :: [Shift] -> Map Guard Int
sumSleepByGuard xs = xs
  <&> (\x -> (guardId x, totalSleep x))
  & M.fromListWith (+)

eventsToSleepRanges :: [LogEntry] -> [Int]
eventsToSleepRanges [] = []
eventsToSleepRanges ((LogEntry FallAsleep t1) : (LogEntry WakeUp t2) : rest) = 
  (todPairToRange t1 t2) <> eventsToSleepRanges rest
eventsToSleepRanges _ = error "Bad events, sleep not followed by wake"

-- hideous date manipulation stripped from time-1.9.2,
-- because stackage has old versions even on nightly,
-- and I don't feel like fighting stack

todToMinutes :: TimeOfDay -> Int
todToMinutes (TimeOfDay h m _) = (h * 60) + m

todPairToRange :: LocalTime -> LocalTime -> [Int]
todPairToRange (LocalTime d1 t1) (LocalTime d2 t2) = case diffDays d1 d2 of
  0 -> [todToMinutes t1 .. todToMinutes t2 - 1]
  1 -> [todToMinutes t1 .. beforeMidnight ] <> [todToMinutes midnight .. todToMinutes t2]
  _ -> error "two day shift, what?"
  where beforeMidnight = todToMinutes (TimeOfDay 23 59 00)

-- boring parsing stuff

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

parseEvent :: Parser Event
parseEvent = choice [wake, sleep, beginShift] where
  wake = string "wakes up" *> pure WakeUp
  sleep = string "falls asleep" *> pure FallAsleep
  beginShift = Begin <$> (string "Guard #" *> int <* string "begins shift")

inputPath :: FilePath
inputPath = "2018\\day4\\input.txt"
