{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Parts where

import           Common
import           Control.Applicative
import           Control.Parallel
import           Control.Parallel.Strategies
import           Control.Monad
import           Data.Foldable
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List (groupBy)
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map(..))
import qualified Data.Sequence as SQ
import           Data.Sequence (Seq(..))
import qualified Data.Set as S
import           Data.Set (Set(..))
import           Data.Traversable
import           Text.RawString.QQ
import           Text.Trifecta hiding (Step)

main :: IO ()
main = do
  steps <- parseInput (many parseStep) [] "2018\\day7\\input.txt"
  putStrLn $ "Input length: " <> (show $ length steps)
  -- mapM_ (putStrLn . show) steps

  let (edgeless, edged) = M.partition null $ ordersWithDepsToSDM steps
  putStrLn "Edged: "
  print $ M.filter (any (\x -> x == 'K')) edged
  putStrLn "Edgeless: "
  print edgeless

  putStrLn "wut"

type Step = Char
type SDM = Map Step [Step] -- step dep map
data StepOrder = StepOrder Step Step -- StepOrder _ must be finished before _
  deriving (Eq, Show, Ord)

getStep (StepOrder _ s) = s
getDep  (StepOrder d _) = d

topoSort :: SDM -> Seq Step
topoSort xs = do
  let (initials, edged) = M.partition null xs
  let l = SQ.empty
  let sortedInitials = fst <$> M.toAscList initials


  undefined
  -- foreach edgeless \n
  --   delete n from edgeless
  --   append n to tail of seq
  --     foreach edged having a reference to n \e, r 
  --       delete reference r from e's map entry
  --       
  
  -- https://en.wikipedia.org/wiki/Topological_sorting

  -- have an empty sequence
  -- 

buildFrom :: Step -> Seq Step -> SDM -> Seq Step
buildFrom node sqn edged = do
  let dependsOnNode = M.filter (any $ (==) node) edged
  if (null dependsOnNode) then sqn else
    undefined

getSetOfAllSteps :: [StepOrder] -> Set Step
getSetOfAllSteps xs = foldr (\x acc -> S.insert (getDep x) acc) steps xs
  where steps = S.fromList $ fmap getStep xs

ordersWithDepsToSDM :: [StepOrder] -> SDM
ordersWithDepsToSDM xs = do
  let groupsByStep = groupBy (\x y -> getStep x == getStep y) xs
  let makeEntries l@(h:_) = (getStep h, fmap getDep l)
  let finals = M.fromList $ makeEntries <$> groupsByStep

  let initials = M.fromList $ fmap (( , []) . getDep) xs
  M.union finals initials

parseStep :: Parser StepOrder
parseStep = do
  string "Step "
  precond <- letter
  string " must be finished before step "
  latter <- letter
  string " can begin."
  whiteSpace
  pure $ StepOrder precond latter

test :: [StepOrder]
test = fromResult [] $ tp (many parseStep) testInput

testInput :: String
testInput = [r|Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.|]
