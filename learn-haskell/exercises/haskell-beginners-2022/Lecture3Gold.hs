module Lecture3Gold where

import Data.List
import Data.Semigroup

validateSemigroup :: (Semigroup a, Eq a) => a -> a -> a -> Bool
validateSemigroup x y z = x <> (y <> z) == (x <> y) <> z

validateMonoid :: (Monoid a, Eq a) => a -> Bool
validateMonoid x = (x <> mempty == x) && (mempty <> x == x)

newtype Gold = Gold
  { unGold :: Int
  }
  deriving (Show, Eq)

-- | Addition of gold coins.
instance Semigroup Gold where
  (<>) :: Gold -> Gold -> Gold
  (<>) (Gold g1) (Gold g2) = Gold (g1 + g2)

instance Monoid Gold where
  mempty :: Gold
  mempty = Gold 0

testGold :: Bool
testGold =
  let x = Gold 10
      y = Gold 20
      z = Gold 30
      test1 = validateSemigroup x y z
      test2 = validateMonoid x
   in test1 && test2

{-
>>> def check_semigroup(diamond):
...     for x in [True, False]:
...         for y in [True, False]:
...             for z in [True, False]:
...                 lhs = diamond(diamond(x, y), z)
...                 rhs = diamond(x, diamond(y, z))
...                 if lhs != rhs:
...                     print(f"({x}<>{y})<>{z} != {x}<>({y}<>{z})")
...
...
...
...
...
>>> check_semigroup(lambda a, b: a or b)
>>> check_semigroup(lambda a, b: a and b)
>>>

As can be seen above, I could've used either and or or as the semigroup op, both work.
-}
data Reward = Reward
  { rewardGold :: Gold,
    rewardSpecial :: Bool
  }
  deriving (Show, Eq)

instance Semigroup Reward where
  (<>) :: Reward -> Reward -> Reward
  (<>) (Reward g1 b1) (Reward g2 b2) = Reward g b
    where
      g = g1 <> g2
      b = b1 || b2

instance Monoid Reward where
  mempty :: Reward
  mempty = Reward (Gold 0) False

testReward :: Bool
testReward =
  let test1 = all id [validateSemigroup (mkReward x) (mkReward y) (mkReward z) | x <- [True, False], y <- [True, False], z <- [True, False]]
      test2 = all id [validateMonoid (mkReward True), validateMonoid (mkReward False)]
   in test1 && test2
  where
    mkReward :: Bool -> Reward
    mkReward = Reward (Gold 10)

-- Gold and Reward are concrete kinds so cannot implement Foldable.