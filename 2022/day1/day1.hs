{-# LANGUAGE OverloadedStrings #-}

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Text.Read qualified as T.Read

main :: IO ()
main = do
  contents <- T.IO.readFile "input"
  let bins = T.splitOn "\n\n" contents
  let sums = sum . map (either undefined fst . T.Read.decimal) . T.lines <$> bins
  let sortedSums = L.sortBy (flip compare) sums
  print . head $ sortedSums
  print . sum $ take 3 sortedSums
