{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module IntCode where


import           Data.Vector.Unboxed         (Vector(..), MVector(..))
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           Control.Monad
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class

-- https://www.schoolofhaskell.com/user/commercial/content/vector
-- https://gist.github.com/chrisdone/d22f41b683e333380c76dbc9c60ed72d
-- https://stackoverflow.com/questions/23579959/how-to-put-mutable-vector-into-state-monad

main :: IO ()
main = do
  instructions <- input
  program <- V.thaw instructions
  res <- stToIO $ flip runReaderT program run
  print res
  
input :: IO (Vector Int)
input = do 
  file <- readFile inputPath -- "1,3,2,0.."
  let bracketed = "[" <> file <> "]"
  pure . V.fromList . read $ bracketed

inputPath :: FilePath
inputPath = "2019\\day2\\input.txt"

data OpCode = Add | Mul | Halt | Error
  deriving (Eq, Show, Ord)

data Frame = Next Int | Success | Exception String
  deriving (Eq, Show)

getOpCode :: Int -> OpCode
getOpCode 1  = Add
getOpCode 2  = Mul
getOpCode 99 = Halt
getOpCode _  = Error

type Program s = ReaderT (MVector s Int) (ST s)

evalFrame :: Frame -> Program s Frame
evalFrame (Next frameIndex) = do
  v <- ask
  opCode        <- MV.read v frameIndex
  let res = getOpCode opCode
  let nextFrame = Next (frameIndex + 4)
  case res of
    Halt  -> pure Success
    Error -> pure (Exception $ "it done broke: code " <> show opCode)
    _     -> do 
      operand1   <- MV.read v (frameIndex + 1) >>= MV.read v
      operand2   <- MV.read v (frameIndex + 2) >>= MV.read v
      outputAddr <- MV.read v (frameIndex + 3)
      case res of
        Add -> MV.write v outputAddr (operand1 + operand2) >> pure nextFrame
        Mul -> MV.write v outputAddr (operand1 * operand2) >> pure nextFrame


restore1202AlarmState :: Program s ()
restore1202AlarmState = ask >>= \v -> MV.write v 1 12 >> MV.write v 2 2

run :: Program s (Vector Int)
run = do
  program <- ask
  restore1202AlarmState
  let initialFrame = Next 0

  let loop f a = do
      n <- f a
      case n of
        Next _      -> loop f n
        Success     -> pure ()
        Exception _ -> error $ show n
  
  loop evalFrame initialFrame
  V.freeze program
