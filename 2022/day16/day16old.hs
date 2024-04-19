{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.State.Lazy
  ( MonadTrans (lift),
    StateT (runStateT),
    gets,
    modify,
  )
import Data.Char qualified as C
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.PriorityQueue.FingerTree (PQueue)
import Data.PriorityQueue.FingerTree qualified as PQueue
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
nAdj vg v = open ++ (wait : move)
  where
    flowAndValve = (v.valve.flow, v.valve)
    wait = (v {elapsed = timeLimit}, (timeLimit - v.elapsed) * v.coRate)
    move = [(v {valve = vg ! n, elapsed = v.elapsed + 1}, v.coRate) | n <- v.valve.adj]
    open = [open' | flowAndValve `Set.member` v.closed]
    open' =
      ( v
          { elapsed = v.elapsed + 1,
            closed = Set.delete flowAndValve (v.closed),
            coRate = v.coRate - v.valve.flow
          },
        v.coRate
      )

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
  { pq :: PQueue Int (Vertex a),
    dist :: HashMap (Vertex a) Int
    -- prev :: HashMap (Vertex a) (Vertex a)
  }
  deriving (Show, Eq, Ord)

-- minCopressure is the min of copressure across paths to goal nodes
-- returns length of path, if one can be found to a goal node
-- copressure is the integral of corate over time,
-- where corate is the total pressure held in by closed valves.
-- implementation: A* algorithm until a goal node is reached
minCopressure :: forall a. (Ord a, Hashable a) => ValveGraph a -> Vertex a -> Maybe Int
minCopressure vg = msum . L.unfoldr (runStateT iter) . dInit
  where
    -- iteration of relaxation
    -- returns Nothing or length of path found on this iteration
    -- fails if the pqueue is empty
    iter :: StateT (SPState a) Maybe (Maybe Int)
    iter = do
      -- get min of pqueue
      ((vDist, v), pq) <- lift . PQueue.minViewWithKey =<< gets (.pq)
      -- pop the queue
      modify $ \d -> d {pq}
      -- relaxation procedure
      let relax = do
            dist <- gets (.dist)
            let relaxed = mapMaybe (tryRelax dist $ vDist - heuristic v) $ nAdj vg v
            let (pq', dist') = (PQueue.fromList *** HashMap.fromList) $ (map swap &&& id) relaxed
            modify $ \d -> d {pq = PQueue.union pq' d.pq, dist = HashMap.union dist' d.dist}
            return Nothing
      -- check up to date
      vDist' <- (! v) <$> gets (.dist)
      if
          | vDist' /= vDist -> return Nothing
          | vIsGoal v -> return $ Just vDist
          | otherwise -> relax
    -- relax a vertex u from a distance vDistAdjusted given weight uvDist and map dist
    tryRelax dist vDistAdjusted (u, uvDist) = do
      let uDist = dist !? u
      let uDist' = uvDist + heuristic u + vDistAdjusted
      guard $ isNothing uDist || uDist' < fromJust uDist
      return (u, uDist')
    -- initial state
    dInit s =
      SPState
        { pq = PQueue.singleton (heuristic s) s,
          dist = HashMap.singleton s (heuristic s)
          -- prev = HashMap.empty
        }

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
