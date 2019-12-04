module Sustain where

import           Text.Trifecta

data Zip = Zip [Bool] Bool [Bool]
  deriving (Eq)

instance Show Zip where
  show (Zip l c r) = "Zip " 
    <> (show $ reverse $ take 3 l) 
    <> (show c) 
    <> (show $ take 3 r)

type Rule = (Bool, Bool, Bool, Bool, Bool)

-- always allowed to add an empty pot to the end of a list

-- parseInput = 
