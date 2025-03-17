power :: (Num a, Integral t) => a -> t -> a
power x 1 = x
power x n
  | n `mod` 2 == 1 = x * p * p
  | otherwise = p * p
  where
    p = power x (n `div` 2)

data Node a = Empty | Node {val :: a, left :: Node a, right :: Node a} deriving (Show)

height :: (Num p, Ord p) => Node a -> p
height Empty = -1
height (Node v l r) = max (height l) (height r) + 1

reach :: (Num p, Ord p) => Node a -> p
reach n
  | ht >= 0 = ht + 1
  | otherwise = 0
  where
    ht = height n

diameter :: (Num b, Ord b) => Node a -> b
diameter Empty = 0
diameter (Node v l r) = maximum [diameter l, diameter r, reach l + reach r]

-- let g = Node 'G' Empty Empty
-- let f = Node 'F' Empty g
-- let e = Node 'E' Empty Empty
-- let c = Node 'C' e f
-- let d = Node 'D' Empty Empty
-- let b = Node 'B' d Empty
-- let a = Node 'A' b c

-- diameter a
