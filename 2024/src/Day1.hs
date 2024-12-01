module Day1 where

import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.TH (makeEffect)

main :: IO ()
main = putStrLn "Hello world!"

data FileSystem :: Effect where
    ReadFileFS :: FilePath -> FileSystem m String
    WriteFileFS :: FilePath -> String -> FileSystem m ()

makeEffect ''FileSystem

-- type instance DispatchOf FileSystem = Dynamic