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

parseLine :: Parsec Void Text (Int, Int)
parseLine = (,) <$> Lexer.decimal <* space <*> Lexer.decimal

getInput :: (FileSystem :> es) => FilePath -> Eff es Text
getInput path = TE.decodeUtf8Lenient <$> EBS.readFile path

main :: IO ()
main = runEff . runFileSystem $ do
  let inputFilename = "data/day1.txt"
  contents <- getInput inputFilename
  let res = error . show ||| id $ parse (many $ parseLine <* newline) inputFilename contents

  print res
