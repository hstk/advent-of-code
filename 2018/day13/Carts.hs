{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Carts where

import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Array.Unboxed
import           Data.Array.IArray
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import           Text.RawString.QQ

main :: IO ()
main = do
  let network = buildTrack sampleInput
  let global = Global (findCarts network) network 0
  let naiveRecursion = runGlobal' global
  print naiveRecursion
  
  putStrLn "haha wut"

data Global = Global
  { carts :: [Cart]
  , track :: Track
  , ticks :: Int
  }

instance Show Global where
  show (Global c _ _) = "Carts: " <> (show $ c)

type Pos = (Int, Int) 

data Direction = U | D | L | R
  deriving (Eq, Show, Enum, Bounded)

data Turn = LeftTurn | Straight | RightTurn
  deriving (Eq, Show, Enum, Bounded)

data Cart = Cart 
  { pos         :: Pos
  , nextTurn    :: Turn
  , dirOfTravel :: Direction 
  } deriving (Eq, Show)

data Trackpiece = V | H | Intersection | Corner Char | None
  deriving (Eq, Show)

type Track = Array Pos Char

buildTrack :: String -> Track
buildTrack xs = let
  width  = length . head . lines $ xs
  height = length . lines $ xs
  dimensions = ((0, 0), (height - 1, width - 1)) 
  in listArray dimensions (filter (\x -> x /= '\n') xs)

charToSegment :: Char -> Trackpiece
charToSegment = \case
  '|'  -> V
  '-'  -> H
  '\\' -> Corner '\\'
  '/'  -> Corner '/'
  '+'  -> Intersection
  'v'  -> V
  '^'  -> V
  '<'  -> H
  '>'  -> H
  _    -> None

findCarts :: Track -> [Cart]
findCarts track = let
  isCart = \case
    '^' -> Just U
    'v' -> Just D
    '<' -> Just L
    '>' -> Just R
    _   -> Nothing
  asList = filter (\(ix, v) -> isJust $ isCart v) $ assocs track
  toCarts = fmap (\(ix, v) -> Cart { pos = ix
                                   , nextTurn = LeftTurn
                                   , dirOfTravel = fromJust $ isCart v})
  in toCarts asList

isCollision :: [Cart] -> Maybe Pos
isCollision xs = listToMaybe . (\x -> x >>= id) . filter (\x -> length x > 1) . group $ fmap pos xs

-- need to sequentially move carts, checking for collision after every move
-- fold that thing
runGlobal :: Global -> (Maybe Pos, Global)
runGlobal g@(Global carts track ticks) = let
  movedCarts = fmap (moveCart track) carts
  in (isCollision movedCarts, Global (movedCarts) track (ticks + 1))

runGlobal' :: Global -> (Pos, Int)
runGlobal' g = case runGlobal g of
  (Just x,  h) -> (x, ticks h)
  (Nothing, h) -> runGlobal' h


moveCart :: Track -> Cart -> Cart
moveCart t (Cart (y,x) nextTurn dir) = do
  let movingTo = case dir of
        U -> (y - 1, x)
        D -> (y + 1, x)
        L -> (y, x - 1)
        R -> (y, x + 1)
  let nextTile = t ! movingTo
  let newDir = case (charToSegment nextTile) of
        V -> dir -- keep on trucking
        H -> dir
        Corner '\\'  -> case dir of
                        R -> D
                        U -> L
                        L -> U
                        D -> R
        Corner '/'   -> case dir of
                        U -> R
                        L -> D
                        R -> U
                        D -> L
        Intersection -> case (nextTurn, dir) of
                        (Straight, x) -> x
                        (LeftTurn, U) -> L
                        (LeftTurn, D) -> R
                        (LeftTurn, L) -> D
                        (LeftTurn, R) -> U
                        (RightTurn, U) -> R
                        (RightTurn, D) -> L
                        (RightTurn, L) -> U
                        (RightTurn, R) -> D
        None         -> error "derailed!"
  let newTurn = case nextTurn of
                      LeftTurn  -> Straight
                      Straight  -> RightTurn
                      RightTurn -> LeftTurn
  Cart movingTo newTurn newDir

sampleInput :: String
sampleInput = drop 1 $ [r|
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   |]

inp = buildTrack sampleInput
theCarts = findCarts inp
g = Global (theCarts) inp 0
mv = moveCart inp

drawCart :: Cart -> Char
drawCart c = case dirOfTravel c of
  U -> '^'
  D -> 'v'
  L -> '<'
  R -> '>'

printTrack :: Track -> [Cart] -> IO ()
printTrack track carts = do
  let (_, (cols, rows)) = bounds track
  let cartRep = fmap (\x -> (pos x, drawCart x)) carts
  let groups = union cartRep (assocs track)
               & groupBy (on (==) (fst . fst))
               & (fmap . fmap) snd
  mapM_ putStrLn groups

pt = printTrack inp