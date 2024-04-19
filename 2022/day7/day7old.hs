import Control.Arrow
import qualified Data.Tree as Tree
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Command a = Cd (Subdir a) -- | Ls a
    deriving (Read, Show, Eq, Ord)
data Subdir a = Root | Parent | Subdir a
    deriving (Read, Show, Eq, Ord)

data Dir a = Dir {dirname :: a, subdirs :: [Dir a], dirfiles :: [File a]}
    deriving (Read, Show, Eq, Ord)
data File a = File {filename :: a, filesize :: Int} -- name, size
    deriving (Read, Show, Eq, Ord)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc = (id *** fst) &&& snd . snd
-- assoc' :: (a, (b, c)) -> ((a, b), c)
-- assoc' (a, (b, c)) = ((a, b), c)

parse :: [T.Text] -> [Command T.Text]
parse = L.unfoldr (Just . (first nextToken . context))
    where nextToken (next, listing) | null listing = Cd Root
                                    | otherwise = Cd Root
          context = assoc . (head &&& (L.break ((=='$') . T.head)) . tail)

main :: IO ()
main = do
    contents <- T.lines <$> T.IO.readFile "input"
    let root = Dir {dirname = "/", subdirs = [], dirfiles = []}
    return ()
