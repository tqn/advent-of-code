import Control.Arrow
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Text.Read qualified as T.Read

parseStacks :: [T.Text] -> [[Char]]
parseStacks = map produceStack . pickRows . prepare
  where
    produceStack = T.unpack . T.stripStart
    pickRows = map snd . filter ((== 1) . (`mod` 4) . fst) . zip [0 ..]
    prepare = T.transpose . init

parseInstr :: T.Text -> (Int, Int, Int)
parseInstr = pickInts . T.words
  where
    pickInts [_, ct, _, fr, _, to] = (readInt ct, readInt fr - 1, readInt to - 1)
    readInt = fst . (undefined ||| id) . T.Read.decimal

listSet :: a -> Int -> [a] -> [a]
listSet value = curry $ insert . uncurry splitAt
  where
    insert (before, _ : after) = before ++ value : after

-- performance?
execInstr :: ([a] -> [a]) -> [[a]] -> (Int, Int, Int) -> [[a]]
execInstr f stacks (ct, from, to) = modifyStacks stacks
  where
    (toStack, fromStack) = first ((++ stacks !! to) . f) $ splitAt ct $ stacks !! from
    modifyStacks = listSet toStack to . listSet fromStack from

main :: IO ()
main = do
  splitContents <- break T.null . T.lines <$> T.IO.readFile "input"
  let (stacks, instrs) = parseStacks *** map parseInstr . tail $ splitContents
  putStrLn $ head <$> L.foldl' (execInstr reverse) stacks instrs
  putStrLn $ head <$> L.foldl' (execInstr id) stacks instrs
