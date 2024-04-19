module Main where

import Control.Arrow
import Data.Char (digitToInt)
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO

visibleL :: (Integral a) => [a] -> [Bool]
visibleL = let r n x = (max n x, n < x) in snd . L.mapAccumL r (-1)

-- visibleL' :: (Integral a) => [a] -> [Bool]
-- visibleL' = snd . L.mapAccumL (curry (uncurry max &&& uncurry (<))) (-1)

visibleR :: (Integral a) => [a] -> [Bool]
visibleR = let r n x = (max n x, n < x) in snd . L.mapAccumR r (-1)

visibleH :: (Integral a) => [a] -> [Bool]
visibleH = map (uncurry (||)) . uncurry zip . (visibleL &&& visibleR)

visibility :: (Integral a) => [[a]] -> [[Bool]]
visibility = map (map (uncurry (||)) . uncurry zip) . uncurry zip . grids
  where
    grids = map visibleH &&& L.transpose . map visibleH . L.transpose

main :: IO ()
main = do
  contents <- T.IO.readFile "input"
  let lines = map digitToInt . T.unpack <$> T.lines contents
  print $ sum $ map (L.length . filter id) $ visibility lines
  return ()
