import Data.Ix qualified as Ix
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO

priority :: Char -> Int
priority c
  | Ix.inRange ('a', 'z') c = Ix.index ('a', 'z') c + 1
  | Ix.inRange ('A', 'Z') c = Ix.index ('A', 'Z') c + 27
  | otherwise = error $ "priority " ++ [c] ++ " not in expected range"

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . L.unfoldr (Just . splitAt n)

main :: IO ()
main = do
  lines <- T.lines <$> T.IO.readFile "input"
  let duplicates = do
        line <- lines
        let (comp0, comp1) = T.splitAt (T.length line `div` 2) line
        return $ head $ L.intersect (T.unpack comp0) (T.unpack comp1)
  print $ sum $ priority <$> duplicates
  let tripleDupes = head . foldr1 L.intersect <$> chunks 3 $ T.unpack <$> lines
  print $ sum $ priority <$> tripleDupes
