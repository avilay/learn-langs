module MyLib (someFunc) where

data BinaryTreeNode =
    EmptyBniaryTreeNode |
    CreateBinaryTreeNode {
        value :: Int,
        left  :: BinaryTreeNode,
        right :: BinaryTreeNode
    }
    deriving Show

someFunc :: IO ()
someFunc = putStrLn "someFunc"
