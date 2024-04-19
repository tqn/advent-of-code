{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.Char qualified as C
import Data.Either
import Data.List (foldl')
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String qualified as String
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T.IO
import Data.Text.Read qualified as T.Read
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- structure of the syntax of the input
data Command a = Cd a | Ls [Listing a]
  deriving (Read, Show, Eq, Ord)

data Listing a = LsFile Int a | LsSubdir a
  deriving (Read, Show, Eq, Ord)

-- used in the filesystem data structure
data File a = File {fname :: a, fsize :: Int}
  -- data File a = File Int a
  deriving (Read, Show, Eq, Ord)

data Dir a = Dir {dname :: a, dcontents :: Map a (Either (File a) (Dir a)), dprops :: DirProps a}
  deriving (Read, Show, Eq, Ord)

newtype DirProps a = DirProps {dsize :: Int}
  deriving (Read, Show, Eq, Ord)

defaultDirProps :: DirProps a
defaultDirProps = DirProps {dsize = 0}

emptyDir :: a -> Dir a
emptyDir dirname = Dir {dname = dirname, dcontents = Map.empty, dprops = defaultDirProps}

interpretListing :: Listing a -> Either (File a) (Dir a)
interpretListing (LsFile s a) = Left $ File {fname = a, fsize = s}
interpretListing (LsSubdir a) = Right $ emptyDir a

dcontentsFromLs :: Ord a => [Listing a] -> Map a (Either (File a) (Dir a))
dcontentsFromLs = Map.fromList . map (((fname ||| dname) &&& id) . interpretListing)

-- parse a nonempty word followed by space
parseWord :: Parsec Void T.Text T.Text
parseWord = takeWhile1P (Just "word") (not . C.isSpace) <* space

-- parse a command in the input file
parseCommand :: Parsec Void T.Text (Command T.Text)
parseCommand = string "$ " >> (pCd <|> pLs)
  where
    pCd = string "cd " >> Cd <$> parseWord
    pLs = string "ls" >> space >> Ls <$> many (pLsSubdir <|> pLsFile)
    pLsSubdir = string "dir " >> LsSubdir <$> parseWord
    pLsFile = LsFile <$> Lexer.decimal <* space <*> parseWord

-- execCommand (path as stack of parent directories) (command) = (updated directories)
execCommand :: (Ord a, Show a, String.IsString a) => [Dir a] -> Command a -> [Dir a]
execCommand [] _ = error "current working directory does not exist"
execCommand ds (Cd "/") = [last ds]
execCommand [_] (Cd "..") = error "cannot `cd ..` in root directory"
execCommand (_ : ds) (Cd "..") = ds
execCommand path@(d : ds) (Cd sname) = case Map.lookup sname (dcontents d) of
  Just (Right s) -> s : path
  Nothing -> scanl updateParentDir (emptyDir sname) path
  _ -> error $ "cannot `cd " ++ show sname ++ "` in " ++ show ds ++ "because it is a file"
execCommand (d : ds) (Ls listing) = scanl updateParentDir d' ds
  where
    d' = d {dcontents = Map.union (dcontents d) (dcontentsFromLs listing)}

-- `updateParentDir s d` is an updated version of d with subdir s
updateParentDir :: Ord a => Dir a -> Dir a -> Dir a
updateParentDir s d = d {dcontents = Map.insert (dname s) (Right s) (dcontents d)}

-- update size
calcDirProps :: (Ord a) => Dir a -> Dir a
calcDirProps d = d {dcontents = contents, dprops = props}
  where
    props = DirProps {dsize = sum $ map (either fsize (dsize . dprops)) $ Map.elems contents}
    contents = Map.map (right calcDirProps) $ dcontents d

-- pre-order traversal
flattenDir :: Dir a -> [Dir a]
flattenDir = uncurry (++) . (return &&& concatMap flattenDir . rights . Map.elems . dcontents)

main :: IO ()
main = do
  let inputFilename = "input"
  contents <- T.IO.readFile inputFilename
  let commands = error . show ||| id $ parse (many parseCommand) inputFilename contents
  let root = calcDirProps $ last $ foldl' execCommand [emptyDir "/"] commands
  print $ sum $ filter (<= 100000) $ map (dsize . dprops) $ flattenDir root
  let usedSpace = dsize . dprops $ root
  let spaceToRemove = usedSpace - 40000000
  print $ minimum $ filter (>= spaceToRemove) $ map (dsize . dprops) $ flattenDir root

main_ :: IO ()
main_ =
  let inputFilename = "input"
   in T.IO.readFile inputFilename
        >>= ( \contents ->
                let commands = error . show ||| id $ parse (many parseCommand) inputFilename contents
                    root = calcDirProps $ last $ foldl' execCommand [emptyDir "/"] commands
                 in print (sum $ filter (<= 100000) $ map (dsize . dprops) $ flattenDir root) >> do
                      let usedSpace = dsize . dprops $ root
                          spaceToRemove = usedSpace - 40000000
                       in print $ minimum $ filter (>= spaceToRemove) $ map (dsize . dprops) $ flattenDir root
            )

main__ :: IO ()
main__ = readContents >>= process
  where
    inputFilename = "input"
    readContents = T.IO.readFile inputFilename
    process contents = part1 >> part2
      where
        commands = error . show ||| id $ parse (many parseCommand) inputFilename contents
        root = calcDirProps $ last $ foldl' execCommand [emptyDir "/"] commands
        part1 = print $ sum $ filter (<= 100000) $ map (dsize . dprops) $ flattenDir root
        usedSpace = dsize . dprops $ root
        spaceToRemove = usedSpace - 40000000
        part2 = print $ minimum $ filter (>= spaceToRemove) $ map (dsize . dprops) $ flattenDir root