module Main where

data BinaryNode a = EmptyBinaryNode | BinaryNode {value :: a, left :: BinaryNode a, right :: BinaryNode a, parent :: BinaryNode a} deriving (Show)
