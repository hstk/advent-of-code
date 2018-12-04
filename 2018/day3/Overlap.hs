{-# LANGUAGE TupleSections #-}

module Overlap where

import           Common
import           Control.Applicative
import           Data.Function ((&))
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map(..))
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet(..))
import           Data.Monoid
import           Text.Trifecta

-- from proj root: 
-- stack ghc -- .\2018\day3\Overlap.hs -O2 -prof -fprof-auto -rtsopts --make -main-is Overlap
-- .\2018\day3\Overlap.exe +RTS -p

data Claim = Claim
  { claimNumber :: Int
  , origin :: Coord
  , area   :: Area
  } deriving (Eq, Show)

type Coord       = (Int, Int)
type Area        = (Int, Int)
type ClaimNumber = Int
type Val         = (Sum Int, [ClaimNumber])
type ClaimMap    = Map Coord Val

main :: IO ()
main = do
  claims <- parseInput (many (parseClaim <* whiteSpace)) [] inputPath
  let asMap = claims >>= toEntries & createMap
  let overlap = M.filter (\x -> fst x > 1) asMap

  putStrLn $ "Overlapping cells: " <> show (length overlap)

  let claimNosWithOverlap = M.elems overlap >>= snd
  let allClaimsSet = IS.fromList $ claimNumber <$> claims
  let overlappingClaimsSet = IS.fromList claimNosWithOverlap
  let noOverlap = IS.difference allClaimsSet overlappingClaimsSet

  putStrLn $ "Non-overlapping claim: #" <> show noOverlap

toEntries :: Claim -> [(Coord, Val)]
toEntries c = c
  & claimToCoordinates
  <&> ( , (Sum 1, [claimNumber c]))

claimToCoordinates :: Claim -> [Coord]
claimToCoordinates c = liftA2 (,) xs ys where
  (x, y) = origin c
  (w, h) = area c
  xs = [x..(x + w - 1)]
  ys = [y..(y + h - 1)]

-- (Monoid a, Monoid b) => (a1, b1) <> (a2, b2) = (a1 <> a2, b1 <> b2)
createMap :: [(Coord, Val)] -> ClaimMap
createMap = M.fromListWith (<>)

-- "#1268 @ 843,461: 26x22"
parseClaim :: Parser Claim
parseClaim = do
  char '#'
  claimNo <- int
  skipMany (space <|> char '@')
  x <- int
  char ','
  y <- int
  skipMany (space <|> colon)
  width <- int
  char 'x'
  height <- int
  pure $ Claim 
    { claimNumber = claimNo
    , origin = (x, y)
    , area = (width, height) }

inputPath :: FilePath
inputPath = "2018\\day3\\input.txt"

