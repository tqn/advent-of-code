import Data.Text qualified as T
import Data.Text.IO qualified as T.IO

data RPS
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Ord, Enum)

score :: RPS -> RPS -> Int
score theirs ours = shapeScore + outcomeScore
  where
    shapeScore = fromEnum ours + 1
    outcomeScore = case mod (fromEnum theirs - fromEnum ours) 3 of 0 -> 3; 1 -> 0; 2 -> 6

main :: IO ()
main = do
  contents <- T.IO.readFile "input"
  let strategy :: [(RPS, RPS)] = do
        [s0, s1] <- T.words <$> T.lines contents
        let theirs = fromEnum (T.head s0) - fromEnum 'A'
        let ours = fromEnum (T.head s1) - fromEnum 'X'
        return (toEnum theirs, toEnum ours)
  let strategyRevised = do
        (theirs, goal) <- strategy
        let diff = fromEnum goal - 1
        let ours = toEnum $ mod (fromEnum theirs + diff) 3
        return (theirs, ours)
  print $ sum $ map (uncurry score) strategy
  print $ show . sum $ map (uncurry score) strategyRevised
