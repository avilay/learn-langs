module Main where

import HaskellSay (haskellSay)
import qualified MyLib (someFunc)

main :: IO ()
main = do
  haskellSay "Hello, Haskell!"
  MyLib.someFunc
