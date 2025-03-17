module Main where

import Control.Monad
import Network.HTTP

fetchWords :: IO ()
fetchWords = do
    resp <- simpleHTTP (getRequest "http://httpbin.org/base64/aGFza2VsbCBmb3IgZXZlcgo=")
    body <- getResponseBody resp
    forM_ (words body) $ \w -> do
        putStr "word: "
        putStrLn w

query :: IO ()
query = do
    putStrLn "Write something!"
    s <- getLine
    let n = length s
    putStrLn ("You wrote " ++ show n ++ " characters.")

askForALine :: IO String
askForALine = do
    putStrLn "Please give me a line"
    getLine

main :: IO ()
main = query