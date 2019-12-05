{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module IntCode where

-- import           Data.Vector                 as V
import           Data.Vector.Unboxed         (Vector(..), MVector(..))
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST
-- https://www.schoolofhaskell.com/user/commercial/content/vector

main :: IO ()
main = do
  program <- input
  let initState = restore1202AlarmState program
  zerp <- VU.thaw initState
  let finalState = runCode initState
  print finalState
  
input :: IO (Vector Int)
input = do 
  file <- readFile inputPath
  let adjusted = "[" <> file <> "]"
  pure . VU.fromList $ read adjusted

inputPath :: FilePath
inputPath = "2019\\day2\\input.txt"

data Frame = Frame 
  { op     :: OpCode
  , params :: (Int, Int)
  , output :: Int
  , nextFrameIndex :: Int
  } deriving (Eq, Show, Ord)

data OpCode = Add | Mult | Halt | Error
  deriving (Eq, Show, Ord)

fromInt :: Int -> OpCode
fromInt 1  = Add
fromInt 2  = Mult
fromInt 99 = Halt
fromInt _  = Error

runCode v = do
  undefined

restore1202AlarmState :: Vector Int -> Vector Int
restore1202AlarmState v = do
  -- mv <- thaw v
  -- zerp <- freeze mv
  undefined

executeInstruction :: Frame -> MVector Int Int -> MVector Int Int
executeInstruction =
  undefined