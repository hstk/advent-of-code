{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}

module IntCode where

import           Data.Vector.Unboxed         (Vector(..), MVector(..))
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           Control.Monad
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST
import           Control.Monad.Except
import           Control.Monad.Reader

main :: IO ()
main = do
  instructions <- input
  program <- V.thaw instructions
  res <- stToIO $ flip runReaderT program runProgram
  putStrLn "Final initial state after running: "
  print $ V.head res

  putStrLn "Trying combos: "
  let valid = runUntilResult instructions
  print valid
  putStrLn "multiply by 100"
  print $ (\(a, b) -> 100 * a + b) <$> valid
  
input :: IO (Vector Int)
input = do 
  file <- readFile inputPath -- "1,3,2,0..", not parseable
  let bracketed = "[" <> file <> "]"
  pure . V.fromList . read $ bracketed

inputPath :: FilePath
inputPath = "2019\\day2\\input.txt"

data OpCode = Add | Mul | Halt | Error
  deriving (Eq, Show, Ord)

data Pointer = Next Int | Success | Exception String
  deriving (Eq, Show)

getOpCode :: Int -> OpCode
getOpCode 1  = Add
getOpCode 2  = Mul
getOpCode 99 = Halt
getOpCode _  = Error

type Program s = ReaderT (MVector s Int) (ST s)

runProgram :: Program s (Vector Int)
runProgram = do
  program <- ask
  restore1202AlarmState
  let initialFrame = Next 0

  let loop f a = do
      n <- f a
      case n of
        Next _      -> loop f n
        Success     -> pure ()
        Exception _ -> error $ show n
  
  loop eval initialFrame
  V.freeze program

restore1202AlarmState :: Program s ()
restore1202AlarmState = setInstructions 12 2

setInstructions :: Int -> Int -> Program s ()
setInstructions noun verb = ask >>= \vec -> MV.write vec 1 noun >> MV.write vec 2 verb

eval :: Pointer -> Program s Pointer
eval (Next iPointer) = do
  vec <- ask
  opCode <- MV.read vec iPointer
  let res = getOpCode opCode
  let nextFrame = Next (iPointer + 4)
  case res of
    Halt  -> pure Success
    Error -> pure (Exception $ "it done broke: code " <> show opCode)
    _     -> do 
      operand1   <- MV.read vec (iPointer + 1) >>= MV.read vec
      operand2   <- MV.read vec (iPointer + 2) >>= MV.read vec
      outputAddr <- MV.read vec (iPointer + 3)

      case res of
        Add -> MV.write vec outputAddr (operand1 + operand2) >> pure (Next $ iPointer + 4)
        Mul -> MV.write vec outputAddr (operand1 * operand2) >> pure (Next $ iPointer + 4)

desiredValue :: Int
desiredValue = 19690720

validInstructions :: [Int]
validInstructions = enumFromTo 0 99

validPrograms :: [(Int, Int)]
validPrograms = do
  a <- validInstructions
  b <- validInstructions
  pure (a, b)

runProgramAndCompare :: Vector Int -> (Int, Int) -> Maybe (Int, Int)
runProgramAndCompare v instr@(a,b) = runST $ V.thaw v >>= \x -> flip runReaderT x $ do
  p <- ask
  setInstructions a b
  let initialFrame = Next 0

  let loop f a = do
      n <- f a
      case n of
        Next _      -> loop f n
        Success     -> pure ()
        Exception _ -> error $ show n

  loop eval initialFrame

  output <- MV.read p 0
  case output == desiredValue of
    True -> pure $ Just (a, b)
    _    -> pure $ Nothing

runUntilResult :: Vector Int -> Maybe (Int, Int)
runUntilResult v = msum $ (runProgramAndCompare v) <$> validPrograms
