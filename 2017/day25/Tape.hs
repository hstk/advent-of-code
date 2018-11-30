module Tape where

import Prelude hiding (read)
import Control.Monad
import Control.Monad.Trans.State

-- https://adventofcode.com/2017/day/25
-- https://adventofcode.com/2017/day/25/input

data Symbol = Zero | One deriving (Eq)

instance Show Symbol where
  show One  = "1"
  show Zero = "0"

data TState = A | B | C | D | E | F deriving (Eq, Show)

data Tape = Tape [Symbol] Symbol [Symbol]
  deriving (Eq)

type CycleCount = Int

type Checksum = Int

data Turing = Turing Tape TState CycleCount Checksum
  deriving (Eq, Show)

instance Show Tape where
  show (Tape l c r) = "Tape " 
    <> (show $ reverse $ take 3 l) 
    <> " " 
    <> (show c) 
    <> " " 
    <> (show $ take 3 r)

blankTape :: Tape
blankTape = Tape (repeat Zero) Zero (repeat Zero)

initialState :: Turing
initialState = Turing blankTape A 0 0

flipState :: TState -> TState
flipState A = B
flipState B = A

moveRight :: Tape -> Tape
moveRight (Tape (lHead:ls) center (rHead:rs)) =
  Tape (center:lHead:ls) rHead rs

moveLeft  :: Tape -> Tape
moveLeft (Tape (l:ls) center r) =
  Tape ls l (center:r)

read :: Tape -> Symbol
read (Tape _ center _) = center

write :: Symbol -> Tape -> Tape
write sym (Tape l _ r) = Tape l sym r

countOnes :: Turing -> Int
countOnes (Turing (Tape l c r) _ acc sum) = sum

act :: Turing -> Turing
act (Turing tape tState acc sum) = do
  let symbol = read tape
  let rule = getRule symbol tState
  let newState = getState symbol tState
  let newSum = getSumChange symbol tState
  Turing (rule tape) (newState) (acc + 1) (newSum sum)

runTuring :: Int -> Turing -> Turing
runTuring times =
  execState $ replicateM times $ state $ \x -> ((), act x)

runTuring' times = do
  put initialState

main = print $ countOnes $ runTuring 12134527 initialState

getRule :: Symbol -> TState -> (Tape -> Tape)
getRule symbol state = case (symbol, state) of
  (Zero, A) -> moveRight . write One
  (One,  A) -> moveLeft  . write Zero
  (Zero, B) -> moveLeft  . write One
  (One,  B) -> moveRight . write One
  (Zero, C) -> moveRight . write One
  (One,  C) -> moveLeft  . write Zero
  (Zero, D) -> moveLeft  . write One
  (One,  D) -> moveLeft  . write One
  (Zero, E) -> moveRight . write One
  (One,  E) -> moveRight . write One
  (Zero, F) -> moveRight . write One
  (One,  F) -> moveRight . write One

getState :: Symbol -> TState -> TState
getState symbol state = case (symbol, state) of
  (Zero, A) -> B
  (One,  A) -> C
  (Zero, B) -> A
  (One,  B) -> C
  (Zero, C) -> A
  (One,  C) -> D
  (Zero, D) -> E
  (One,  D) -> C
  (Zero, E) -> F
  (One,  E) -> A
  (Zero, F) -> A
  (One,  F) -> E

getSumChange :: Symbol -> TState -> (Int -> Int)
getSumChange symbol state = case (symbol, state) of
  (Zero, A) -> add
  (One,  A) -> sub
  (Zero, B) -> add
  (One,  B) -> id
  (Zero, C) -> add
  (One,  C) -> sub
  (Zero, D) -> add
  (One,  D) -> id
  (Zero, E) -> add
  (One,  E) -> id
  (Zero, F) -> add
  (One,  F) -> id
  where add  = \x -> x + 1
        sub  = subtract 1
