module Exercism where

append :: Char -> String -> String
append char acc = acc ++ [char]

reverse :: String -> String
reverse = foldr append ""

