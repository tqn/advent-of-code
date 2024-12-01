{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Relude

import Control.Arrow
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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

process :: (Ord a, Num a) => [(a, a)] -> a
process pairs = unzip pairs & (sort *** sort) & uncurry zip <&> dist & sum
 where
  dist (a, b) = abs (a - b)

parseLine :: Parsec Void Text (Int, Int)
parseLine = (,) <$> Lexer.decimal <* space <*> Lexer.decimal

program :: (FileSystem :> es) => FilePath -> Eff es Text
program inputFilename = do
  contents <- TE.decodeUtf8Lenient <$> EBS.readFile inputFilename
  let pairs = error . show ||| id $ parse (many $ parseLine <* newline) inputFilename contents
  return $ show $ process pairs

main :: IO ()
main = runEff . runFileSystem $ print =<< program "data/day1.txt"
