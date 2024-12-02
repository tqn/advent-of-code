module Main where

import Relude

import Control.Arrow hiding ((&&&))
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString qualified as EBS
import Text.Megaparsec (Parsec, option, parse, sepBy1, sepEndBy)
import Text.Megaparsec.Char (hspace1, newline)
import Text.Megaparsec.Char.Lexer qualified as Lexer

process1 :: (Ord a, Num a) => [[a]] -> Int
process1 = length . filter safe

diffs :: (Num a) => [a] -> [a]
diffs (x : xs@(y : _)) = (x - y) : diffs xs
diffs _ = []

safe :: (Ord a, Num a) => [a] -> Bool
safe line = any (all safeDiff) [diffLine, negate <$> diffLine]
 where
  diffLine = diffs line
  safeDiff x = x >= 1 && x <= 3

process2 :: (Ord a, Num a) => [[a]] -> Int
process2 = length . filter safe2

-- Horribly inefficient, but whatever.
safe2 :: (Ord a, Num a) => [a] -> Bool
safe2 line = safe line || any (safe . uncurry (++)) (zip (inits line) (drop 1 (tails line)))

parseLine :: Parsec Void Text [Int]
parseLine = sepBy1 Lexer.decimal hspace1

program :: (FileSystem :> es) => FilePath -> Eff es (Int, Int)
program inputFilename = do
  contents <- TE.decodeUtf8Lenient <$> EBS.readFile inputFilename
  let lines = error . show ||| id $ parse (sepEndBy parseLine newline) inputFilename contents
  return $ process1 &&& process2 $ lines

main :: IO ()
main = runEff . runFileSystem $ do
  (p1, p2) <- program "data/day2.txt"
  print p1
  print p2