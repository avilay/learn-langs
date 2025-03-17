{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use foldl" #-}

-- |
-- Module                  : Lecture2
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 2 of the Haskell Beginners course.
--
-- As in the previous section, implement functions and provide type
-- signatures. If the type signature is not already written, write the
-- most polymorphic type signature you can.
--
-- Unlike exercises to Lecture 1, this module also contains more
-- challenging exercises. You don't need to solve them to finish the
-- course but you can if you like challenges :)
module Lecture2
  ( -- * Normal
    lazyProduct,
    duplicate,
    removeAt,
    evenLists,
    dropSpaces,
    Knight (..),
    dragonFight,

    -- * Hard
    isIncreasing,
    merge,
    mergeSort,
    Expr (..),
    Variables,
    EvalError (..),
    eval,
    constantFolding,
  )
where

import Data.Maybe (isNothing)

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- | Implement a function that finds a product of all the numbers in
-- the list. But implement a lazier version of this function: if you see
-- zero, you can stop calculating product and return 0 immediately.
--
-- >>> lazyProduct [4, 3, 7]
-- 84
lazyProduct :: [Int] -> Int
lazyProduct [] = 0
lazyProduct list = go 1 list
  where
    go :: Int -> [Int] -> Int
    go acc [] = acc
    go acc (0 : xs) = 0
    go acc (x : xs) = go (acc * x) xs

-- | Implement a function that duplicates every element in the list.
--
-- >>> duplicate [3, 1, 2]
-- [3,3,1,1,2,2]
-- >>> duplicate "cab"
-- "ccaabb"
duplicate :: [a] -> [a]

dup x = [x, x]

flatten acc [] = acc
flatten acc (pair : pairs) = flatten (acc ++ pair) pairs

duplicate = flatten [] . map dup

-- | Implement function that takes index and a list and removes the
-- element at the given position. Additionally, this function should also
-- return the removed element.
--
-- >>> removeAt 0 [1 .. 5]
-- (Just 1,[2,3,4,5])
--
-- >>> removeAt 10 [1 .. 5]
-- (Nothing,[1,2,3,4,5])
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt idx list
  | idx >= length list = (Nothing, list)
  | otherwise = (Just x, left ++ right)
  where
    x = list !! idx
    left = take idx list
    right = drop (idx + 1) list

-- | Write a function that takes a list of lists and returns only
-- lists of even lengths.
--
-- >>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
-- [[3,1,2,7],[]]
--
-- ♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
--  in this function.
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

-- | The @dropSpaces@ function takes a string containing a single word
-- or number surrounded by spaces and removes all leading and trailing
-- spaces.
--
-- >>> dropSpaces "   hello  "
-- "hello"
-- >>> dropSpaces "-200            "
-- "-200"
--
-- ♫ NOTE: As in the previous task, use eta-reduction and function
--  composition (the dot (.) operator) in this function.
--
-- f . g . h = f(g(h(x))) = f(g.h)
-- 🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
dropSpaces :: String -> String

ltrim = span (== ' ')

dropSpaces = reverse . snd . ltrim . reverse . snd . ltrim

-- |
--
-- The next task requires to create several data types and functions to
-- model the given situation.
--
-- An evil dragon attacked a village of innocent citizens! After
-- returning to its lair, the dragon became hungry and ate one of its
-- treasure chests by accident.
--
-- The guild in the village found a brave knight to slay the dragon!
-- As a reward, the knight can take the treasure chest.
--
-- Below is the description of the fight and character specifications:
--
--   * A chest contains a non-zero amount of gold and a possible treasure.
--     When defining the type of a treasure chest, you don't know what
--     treasures it stores inside, so your chest data type must be able
--     to contain any possible treasure.
--   * As a reward, the knight takes all the gold, the treasure and experience.
--   * Experience is calculated based on the dragon type. A dragon can be
--     either red, black or green.
--   * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
--   * Stomachs of green dragons contain extreme acid and they melt any
--     treasure except gold. So green dragons have only gold as reward.
--     All other dragons always contain treasure in addition to gold.
--   * Knight tries to slay a dragon with their sword. Each sword strike
--     decreases dragon health by the "sword attack" amount. When the
--     dragon health becomes zero or less, a dragon dies and the knight
--     takes the reward.
--   * After each 10 sword strikes, the dragon breathes fire and decreases
--     knight health by the amount of "dragon fire power". If the
--     knight's health becomes 0 or less, the knight dies.
--   * Additionally, each sword strike decreases "knight's endurance" by one.
--     If a knight's endurance becomes zero, they become tired and are not
--     able to continue the fight so they run away.
--
-- Implement data types to describe treasure, knight and dragon.
-- And implement a function that takes a knight and a dragon and returns
-- one of the three possible fight outcomes.
--
-- You're free to define any helper functions.
--
-- 🕯 HINT: If you find the description overwhelming to implement entirely
--   from scratch, try modelling the problem in stages.
--
--     1. Implement all custom data types without using polymorphism.
--     2. Add @newtype@s for safety where you think is appropriate.
--     3. Encode the fight result as a sum type.
--     4. Add polymorphism.
--     5. Make all invalid states unrepresentable. Think, how you can
--        change your types to prevent green dragons from having any
--        treasure besides gold (if you already haven't done this).

-- some help in the beginning ;)
data Knight = Knight
  { knightHealth :: Health,
    knightAttack :: Int,
    knightEndurance :: Int
  }
  deriving (Show)

type Gold = Int

type FirePower = Int

type Health = Int

data Chest a = Chest
  { chestGold :: Gold,
    chestTreasure :: Maybe a
  }
  deriving (Show)

data DragonColor = Red | Black | Green deriving (Show, Eq)

data Dragon a
  = Dragon
  { dragonColor :: DragonColor,
    dragonPoints :: Int,
    dragonChest :: Chest a,
    dragonFirePower :: FirePower,
    dragonHealth :: Health
  }

createDragon :: DragonColor -> Chest a -> FirePower -> Health -> Dragon a
createDragon Green (Chest gold Nothing) firePower health =
  Dragon
    { dragonColor = Green,
      dragonPoints = 250,
      dragonChest = Chest gold Nothing,
      dragonFirePower = firePower,
      dragonHealth = health
    }
createDragon Green _ _ _ = error "Green dragons cannot have treasures!"
createDragon Black chest firePower health =
  Dragon
    { dragonColor = Black,
      dragonPoints = 150,
      dragonChest = chest,
      dragonFirePower = firePower,
      dragonHealth = health
    }
createDragon Red chest firePower health =
  Dragon
    { dragonColor = Red,
      dragonPoints = 100,
      dragonChest = chest,
      dragonFirePower = firePower,
      dragonHealth = health
    }

-- As a reward, the knight takes all the gold, the treasure and experience.
data Reward a = Reward
  { rewardGold :: Gold,
    rewardTreasure :: Maybe a,
    rewardExperience :: Int
  }

createReward :: Dragon a -> Reward a
createReward (Dragon _ points chest _ _) =
  Reward
    { rewardGold = chestGold chest,
      rewardTreasure = chestTreasure chest,
      rewardExperience = points
    }

-- dragonFight :: Knight -> Dragon -> Reward
-- Each sword strike
--     decreases dragon health by the "sword attack" amount.
-- After each 10 sword strikes, the dragon breathes fire and decreases
--     knight health by the amount of "dragon fire power".
-- Additionally, each sword strike decreases "knight's endurance" by one.
swordStrike :: Int -> Knight -> Dragon a -> (Knight, Dragon a)
swordStrike numStrikes (Knight knightHealth attack endurance) (Dragon color points chest firePower dragonHealth)
  | numStrikes == 10 =
      ( Knight (knightHealth - firePower) attack (endurance - 1),
        Dragon color points chest firePower (dragonHealth - attack)
      )
  | otherwise =
      ( Knight knightHealth attack (endurance - 1),
        Dragon color points chest firePower (dragonHealth - attack)
      )

-- When the
--     dragon health becomes zero or less, a dragon dies and the knight
--     takes the reward.
-- If the
--     knight's health becomes 0 or less, the knight dies.
--     If a knight's endurance becomes zero, they become tired and are not
--     able to continue the fight so they run away.

data EndGame = KnightWins | KnightRuns | KnightDies deriving (Show)

dragonFight :: Int -> Knight -> Dragon a -> (EndGame, Maybe (Reward a))
dragonFight strikes knight dragon
  | knightHealth knight' <= 0 = (KnightDies, Nothing)
  | knightEndurance knight' <= 0 = (KnightRuns, Nothing)
  | dragonHealth dragon' <= 0 = (KnightWins, Just (createReward dragon))
  | otherwise = dragonFight (strikes + 1) knight' dragon'
  where
    (knight', dragon') = swordStrike (strikes + 1) knight dragon

-- dragonFight strikes knight dragon =
--   let (knight', dragon') = swordStrike (strikes + 1) knight dragon
--    in if knightHealth knight' <= 0
--         then (KnightDies, Nothing)
--         else
--           if knightEndurance knight' <= 0
--             then (KnightRuns, Nothing)
--             else
--               if dragonHealth dragon' <= 0
--                 then (KnightWins, Just (createReward dragon))
--                 else dragonFight (strikes + 1) knight' dragon'

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

-- | Write a function that takes a list of numbers and returns 'True'
-- if all the numbers are in the increasing order (i.e. the list is
-- sorted).
--
-- >>> isIncreasing [3, 1, 2]
-- False
-- >>> isIncreasing [1 .. 10]
-- True
isIncreasing :: [Int] -> Bool
isIncreasing [] = False
isIncreasing list = all (\pair -> fst pair <= snd pair) (zip list (tail list))

-- | Implement a function that takes two lists, sorted in the
-- increasing order, and merges them into new list, also sorted in the
-- increasing order.
--
-- The lists are guaranteed to be given sorted, so you don't need to
-- verify that.
--
-- >>> merge [1, 2, 4] [3, 7]
-- [1,2,3,4,7]
merge :: [Int] -> [Int] -> [Int]
merge = go []
  where
    go :: [Int] -> [Int] -> [Int] -> [Int]
    go acc [] [] = acc
    go acc [] l2 = acc ++ l2
    go acc l1 [] = acc ++ l1
    go acc l1 l2
      | head l1 < head l2 = go (acc ++ [head l1]) (tail l1) l2
      | otherwise = go (acc ++ [head l2]) l1 (tail l2)

-- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
-- function takes a list of numbers and returns a new list containing the
-- same numbers but in the increasing order.
--
-- The algorithm of merge sort is the following:
--
--  1. If the given list has less than 2 elements, it's already sorted.
--  2. Otherwise, split list into two lists of the same size.
--  3. Sort each of two lists recursively.
--  4. Merge two resulting sorted lists to get a new sorted list.
--
-- >>> mergeSort [3, 1, 2]
-- [1,2,3]
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list =
  let mid = length list `div` 2
      left = mergeSort (take mid list)
      right = mergeSort (drop mid list)
   in merge left right

-- | Haskell is famous for being a superb language for implementing
-- compilers and interpreters to other programming languages. In the next
-- tasks, you need to implement a tiny part of a compiler.
--
-- We're going to work on a small subset of arithmetic operations.
--
-- In programming we write expressions like "x + 1" or "y + x + 10".
-- Such expressions can be represented in a more structured way (than a
-- string) using the following recursive Algebraic Data Type:
data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

-- | We want to evaluate such expressions. We can associate a value
-- with a variable using a list of pairs.
--
-- You can use the @lookup@ function to search in this list by a variable name:
--
-- * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
type Variables = [(String, Int)]

-- | Unfortunately, it's not guaranteed that variables in our @Expr@
-- data type are present in the given list. So we're going to introduce a
-- separate data for possible evaluation errors.
--
-- Normally, this would be a sum type with several constructors
-- describing all possible errors. But we have only one error in our
-- evaluation process.
data EvalError = VariableNotFound String deriving (Show, Eq)

-- | Having all this set up, we can finally implement an evaluation function.
-- It returns either a successful evaluation result or an error.
-- unbox (Just x) = x
-- unbox Nothing = error "This should not be called."
toEither _ (Just x) = Right x
toEither errMsg Nothing = Left (VariableNotFound errMsg)

eval :: Variables -> Expr -> Either EvalError Int
eval vars (Var x) = toEither (x ++ " is not defined!") (lookup x vars)
eval vars (Lit x) = Right x
eval vars (Add x y) = add (eval vars x) (eval vars y)

add (Left x) _ = Left x
add _ (Left y) = Left y
add (Right x) (Right y) = Right (x + y)

-- | Compilers also perform optimizations! One of the most common
-- optimizations is "Constant Folding". It performs arithmetic operations
-- on all constants known during compile time. This way you can write
-- more verbose and clear code that works as efficient as its shorter
-- version.
--
-- For example, if you have an expression:
--
-- x + 10 + y + 15 + 20
-- Add (Var "x") (Add (Lit 10) (Add (Var "y") (Add (Lit 15) (Lit 20))))
--
-- The result of constant folding can be:
--
-- x + y + 45
--
-- It also can be:
--
-- x + 45 + y
--
-- Write a function that takes and expression and performs "Constant
-- Folding" optimization on the given expression.
constantFolding :: Expr -> Expr
constantFolding expr =
  let (vars, lits) = deconstruct ([], []) expr
      lit = addAll (Lit 0) lits
   in reconstruct lit vars

deconstruct (vars, lits) (Var x) = (Var x : vars, lits)
deconstruct (vars, lits) (Lit x) = (vars, Lit x : lits)
deconstruct (vars, lits) (Add x y) = deconstruct (deconstruct (vars, lits) x) y

reconstruct acc [] = acc
reconstruct acc (var : vars) = reconstruct (Add var acc) vars

addAll acc [] = acc
addAll (Lit acc) (Lit x : lits) = addAll (Lit (acc + x)) lits