{-# LANGUAGE OverloadedStrings #-}

-- import qualified Control.Monad as M

import Data.Either qualified as E
import Data.Ix qualified as Ix
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Text.Read qualified as T.Read

main :: IO ()
main = do
  lines <- T.lines <$> T.IO.readFile "input"
  let parsedLines = [map fst $ E.rights $ T.Read.decimal <$> (T.splitOn "-" =<< T.splitOn "," line) | line <- lines]
  let incidence reducer [v0, v1, v2, v3] = reducer (Ix.inRange (v0, v1)) [v2, v3] || reducer (Ix.inRange (v2, v3)) [v0, v1]
  print $ length $ filter (incidence all) parsedLines
  print $ length $ filter (incidence any) parsedLines

-- equivalent to the last two lines
-- M.sequence_ [ putStrLn $ show $ length $ filter (incidence r) parsedLines | r <- [all, any] ]
