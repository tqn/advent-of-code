{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Arrow
import Control.Monad (msum)
import Control.Monad.State.Lazy
  ( MonadTrans (lift),
    StateT (runStateT),
    gets,
    modify,
  )
import Data.Char qualified as C
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as HashPSQ
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable, hash)
import Data.List qualified as L
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String qualified as String
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Text.Read qualified as T.Read
import Data.Tuple (swap)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P),
    Parsec,
    many,
    parse,
    sepBy,
    try,
    (<|>),
  )
import Text.Megaparsec.Byte qualified as Lexer
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- time limit, constant
timeLimit :: Int
timeLimit = 30

-- syntax defining a valve: name, flow rate, adjacency list
data Valve a = Valve {name :: a, flow :: Int, adj :: [a]}
  deriving (Read, Show, Eq, Ord, Generic, Hashable)

-- improve readability
type ValveGraph a = HashMap a (Valve a)

-- name, flow rate
-- data Valve a = Valve a Int
--   deriving (Read, Show, Eq, Ord)

-- valve = what valve we are at
-- elapsed = how much time has elapsed
-- closed = (flow rate, valve) which valves are closed. first val to sort against for heuristics
-- coRate = sum of rate of closed valves (to save on calculations)
data Vertex a = Vertex {valve :: Valve a, elapsed :: Int, closed :: Set (Int, Valve a), coRate :: Int}
  deriving (Read, Show, Eq, Ord, Generic, Hashable)

--- adjacency list of a vertex, along with weights
-- in `nAdj g v`, `g` maps valve names to the Valve record of `v`
-- returns adjacent vertices and edge weights to them
-- should support the following actions:
-- 1. wait until timeLimit
-- 2. move to adjacent valve, taking 1 minute
-- 3. open this valve if it is currently closed, taking 1 minute
nAdj :: (Ord a, Hashable a) => ValveGraph a -> Vertex a -> [(Vertex a, Int)]
nAdj vg v = open ++ move ++ wait
  where
    flowAndValve = (v.valve.flow, v.valve)
    wait = [(v {elapsed = timeLimit}, (timeLimit - v.elapsed) * v.coRate) | v.elapsed < timeLimit]
    move = [(v {valve = vg ! n, elapsed}, v.coRate) | n <- v.valve.adj]
    open = [open' | flowAndValve `Set.member` v.closed]
    open' =
      ( v
          { elapsed,
            closed = Set.delete flowAndValve (v.closed),
            coRate = v.coRate - v.valve.flow
          },
        v.coRate
      )
    elapsed = v.elapsed + 1

-- is this vertex a goal?
vIsGoal :: Vertex a -> Bool
vIsGoal = (>= timeLimit) . (.elapsed)

-- starting vertex
vInit :: (Ord a, Hashable a, String.IsString a) => ValveGraph a -> Vertex a
vInit vg =
  Vertex
    { valve = vg ! "AA",
      elapsed = 0,
      closed = Set.fromList $ zip flows valves,
      coRate = sum flows
    }
  where
    valves = HashMap.elems vg
    flows = (.flow) <$> valves

-- if every valve was opened in sequence, starting from the largest
heuristic :: Vertex a -> Int
heuristic = uncurry bestcase . ((.elapsed) &&& map fst . Set.toDescList . (.closed))
  where
    bestcase :: Int -> [Int] -> Int
    bestcase elapsed = sum . zipWith (*) openDurations
      where
        openDurations = [1 .. remaining] ++ repeat remaining
        remaining = timeLimit - elapsed

-- shortest path state
data SPState a = SPState
  { pq :: HashPSQ (Vertex a) Int (),
    visited :: HashSet Int
  }
  deriving (Show, Eq)

-- minCopressure is the min of copressure across paths to goal nodes
-- returns length of path, if one can be found to a goal node
-- copressure is the integral of corate over time,
-- where corate is the total pressure held in by closed valves.
-- implementation: A* algorithm until a goal node is reached
-- graph is a DAG so backtracking is impossible
minCopressure :: forall a. (Ord a, Hashable a) => ValveGraph a -> Vertex a -> Maybe Int
minCopressure vg = msum . L.unfoldr (runStateT iter) . dInit
  where
    -- iteration of relaxation
    -- returns Nothing or length of path found on this iteration
    -- fails if the pqueue is empty
    iter :: StateT (SPState a) Maybe (Maybe Int)
    iter = do
      -- get min of pqueue
      ~(v, vDist, _, pq) <- lift . HashPSQ.minView =<< gets (.pq)
      -- pop the queue
      modify $ \s -> s {pq}
      -- since we are decreasing key, v is always up to date
      if vIsGoal v
        then return $ Just vDist
        else do
          visited <- HashSet.insert (hash v) <$> gets (.visited)
          -- neighbors and total distance to start with last step from v
          let newNs = filter (not . (`HashSet.member` visited) . hash . fst) $ nAdj vg v
          let nDists = (fst &&& (vDist - heuristic v +) . uncurry (+) . first heuristic) <$> newNs
          modify $ \s ->
            s
              { pq = L.foldl' (flip $ uncurry decreasePriority) s.pq nDists,
                visited
              }
          return Nothing
    -- initial state
    dInit s =
      SPState
        { pq = HashPSQ.singleton s (heuristic s) (),
          visited = HashSet.empty
        }

-- insert if non-existent or the priority is lower
decreasePriority :: forall v p. (Hashable v, Ord v, Ord p) => v -> p -> HashPSQ v p () -> HashPSQ v p ()
decreasePriority v p = snd . HashPSQ.alter (const () &&& Just . (id &&& const ()) . maybe p (min p . fst)) v

-- parse a nonempty word followed by space
parseValveName :: Parsec Void T.Text T.Text
parseValveName = takeWhile1P (Just "valve name") C.isAlpha <* space

-- parse a command in the input file
parseValve :: Parsec Void T.Text (Valve T.Text)
parseValve = do
  string "Valve "
  name <- parseValveName
  string "has flow rate="
  flow <- Lexer.decimal <* space
  try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve "
  adj <- parseValveName `sepBy` string ", " <* space
  return $ Valve {name = name, flow = flow, adj = adj}

main :: IO ()
main = do
  let inputFilename = "input"
  contents <- T.IO.readFile inputFilename
  -- calculate graph of valves and tunnels
  let valves = error . show ||| id $ parse (many parseValve) inputFilename contents
  let valveGraph = HashMap.fromList $ map ((.name) &&& id) valves
  -- calculate max rate of all valves closed = corate of all valves being closed
  let maxRate = sum $ map (.flow) valves
  print $ (maxRate * timeLimit -) <$> minCopressure valveGraph (vInit valveGraph)
