{-# LANGUAGE OverloadedStrings #-}

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Text.Read qualified as T.Read

main :: IO ()
main = do
  contents <- T.IO.readFile "input"
  let bins = T.splitOn "\n\n" contents
  let sums = do
        bin <- map T.Read.decimal . T.lines <$> bins
        return $ sum $ either undefined fst <$> bin
  -- return $ sum $ do
  --   Right (v :: Int, _) <- bin
  --   return v
  let sortedSums = L.sortBy (flip compare) sums
  print . head $ sortedSums
  print . sum $ take 3 sortedSums
