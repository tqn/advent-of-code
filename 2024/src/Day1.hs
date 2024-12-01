{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Relude

import Control.Arrow hiding ((&&&))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString qualified as EBS
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Byte qualified as Lexer
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

process1 :: (Ord a, Num a) => [(a, a)] -> a
process1 pairs = zip (sort xs) (sort ys) <&> dist & sum
 where
  (xs, ys) = unzip pairs
  dist (a, b) = abs (a - b)

process2 :: (Ord a, Num a) => [(a, a)] -> a
process2 pairs = xs <&> similarity & sum
 where
  (xs, ys) = unzip pairs
  ycounts = foldl' (\m y -> M.insertWith (+) y 1 m) M.empty ys
  similarity x = M.lookup x ycounts & fromMaybe 0 & (* x)

parseLine :: Parsec Void Text (Int, Int)
parseLine = (,) <$> Lexer.decimal <* space <*> Lexer.decimal

program :: (FileSystem :> es) => FilePath -> Eff es (Int, Int)
program inputFilename = do
  contents <- TE.decodeUtf8Lenient <$> EBS.readFile inputFilename
  let pairs = error . show ||| id $ parse (many $ parseLine <* newline) inputFilename contents
  return $ process1 &&& process2 $ pairs

main :: IO ()
main = runEff . runFileSystem $ do
  (p1, p2) <- program "data/day1.txt"
  print p1
  print p2
