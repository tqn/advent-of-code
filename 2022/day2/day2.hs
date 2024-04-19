{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as T.IO

score :: Int -> Int -> Int
score theirs ours = shapeScore + outcomeScore
  where
    shapeScore = ours + 1
    outcomeScore = case mod (theirs - ours) 3 of 0 -> 3; 1 -> 0; 2 -> 6

main :: IO ()
main = do
  contents <- T.IO.readFile "input"
  let strategy = do
        [s0, s1] <- T.words <$> T.lines contents
        let theirs = fromEnum (T.head s0) - fromEnum 'A'
        let ours = fromEnum (T.head s1) - fromEnum 'X'
        return (theirs, ours)
  let strategyRevised = do
        (theirs, goal) <- strategy
        let diff = goal - 1
        let ours = mod (theirs + diff) 3
        return (theirs, ours)
  print $ sum $ map (uncurry score) strategy
  print $ sum $ map (uncurry score) strategyRevised
