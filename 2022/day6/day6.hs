import Control.Arrow ((&&&))
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO

windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((>= n) . length) . L.unfoldr (Just . (take n &&& tail))

-- original, O(n * width^2)
indOfUnique :: Eq a => Int -> [a] -> Maybe Int
indOfUnique width = fmap (+ width) . L.elemIndex width . map length . L.nub . windows width

-- faster O(n * width log width)
indOfUnique' :: Ord a => Int -> [a] -> Maybe Int
indOfUnique' width = fmap (+ width) . L.elemIndex width . map (Set.size . Set.fromList) . windows width

main :: IO ()
main = do
  contents <- T.unpack . head . T.lines <$> T.IO.readFile "input"
  print $ indOfUnique' 4 contents
  print $ indOfUnique' 14 contents
