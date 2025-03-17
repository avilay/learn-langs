module Lecture3Treasure where

issg :: (Semigroup a, Eq a) => a -> a -> a -> Bool
issg x y z = x <> (y <> z) == (x <> y) <> z

ismonoid :: (Monoid a, Eq a) => a -> Bool
ismonoid x = (x <> mempty == x) && (mempty <> x == x)

data Treasure a
  = NoTreasure
  | SomeTreasure a
  deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Treasure a) where
  (<>) :: (Semigroup a) => Treasure a -> Treasure a -> Treasure a
  (<>) NoTreasure NoTreasure = NoTreasure
  (<>) NoTreasure (SomeTreasure x) = SomeTreasure x
  (<>) (SomeTreasure x) NoTreasure = SomeTreasure x
  (<>) (SomeTreasure x) (SomeTreasure y) = SomeTreasure (x <> y)

instance (Semigroup a) => Monoid (Treasure a) where
  mempty :: Treasure a
  mempty = NoTreasure