module Challenge where

import           Common
import           Control.Applicative
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List (sort, groupBy)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map(..))
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap(..))
import           Data.Time
import           Data.Time.LocalTime
import           Data.Time.Calendar
import           Text.Trifecta

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
  , totalSleep :: SleepMinutes
  } deriving (Eq, Show)

type Guard = Int
type SleepMinutes = Int
data Status = Sleeping Guard | Awake Guard
data Event = FallAsleep | WakeUp | Begin Guard
  deriving (Eq, Show)


main :: IO ()
main = do
  logs <- sort <$> parseInput (many (parseLog <* whiteSpace)) [] inputPath
  let logsByShift = groupByShift logs
  let shifts = processShift <$> logsByShift


  putStrLn "wut"

groupByShift :: [LogEntry] -> [[LogEntry]]
groupByShift = groupBy sameShift

sameShift :: LogEntry -> LogEntry -> Bool
sameShift a (LogEntry b _) = case b of
  Begin _ -> False
  _       -> True

summarizeSleepByGuard :: [Shift] -> Map Guard SleepMinutes
summarizeSleepByGuard xs = xs
  <&> (\x -> (guardId x, totalSleep x))
  & M.fromListWith (+)

processShift :: [LogEntry] -> Shift
processShift ((LogEntry (Begin guard) startTime):events) = do
  let sleepTime = processSleep events
  Shift { guardId = guard
        , start = startTime
        , events = events
        , sleepPeriods = sleepTime
        , totalSleep = sum sleepTime }
processShift _ = error "first event isn't a shift start, bad grouping"

processSleep :: [LogEntry] -> [Int]
processSleep [] = []

processSleep ((LogEntry FallAsleep t1) : (LogEntry WakeUp t2) : rest) = 
  (diffTimesInMin t1 t2) : processSleep rest

processSleep _ = error "Bad events, sleep not followed by wake"

-- hideous date manipulation stripped from time-1.9.2,
-- because stackage has old versions even on nightly,
-- and I don't feel like fighting stack

addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

diffTimesInMin :: LocalTime -> LocalTime -> Int
diffTimesInMin t1 t2 = do
  let diff = diffLocalTime t2 t1
  let (LocalTime c t) = addLocalTime diff (LocalTime (fromGregorian 0 1 1) midnight) -- can't create 0000/0/0 AD
  let (_, _, daysDiff) = toGregorian c
  let (TimeOfDay hoursDiff minutesDiff _) = t
  let daysDiffInMin  = 24 * 60 * (daysDiff - 1)
  let hourDiffInMin = 60 * hoursDiff
  daysDiffInMin + hourDiffInMin + minutesDiff

todToMinutes :: LocalTime -> Int
todToMinutes (LocalTime _ (TimeOfDay h m _)) = (h * 60) + m

todPairToRange :: LocalTime -> LocalTime -> [Int]
todPairToRange a b = [todToMinutes a .. todToMinutes b - 1]

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
