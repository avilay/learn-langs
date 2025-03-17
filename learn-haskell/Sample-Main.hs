module Main where

-- remember to rename this file to Main.hs before compiling.

newtype ModelId = ModelId Int deriving (Show)

deploy :: ModelId -> Integer -> String
deploy modelId numShards = "Deploying model " ++ show modelId ++ " on " ++ show numShards ++ " shards."

main :: IO ()
main = do
  let modelId = ModelId 10
      numShards = 16
      repr = deploy modelId numShards
   in putStrLn repr
