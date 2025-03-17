module Example where

import Distribution.Simple.Utils (xargs)

increment :: (Num a) => a -> a
increment x = x + 1

whoami name
  | name == "avilay" = "YOU"
  | name == "a" = "COULD BE YOU"
  | otherwise = "NOT YOU"

tp x =
  let y = 10 * x
      z = y + 10
   in y + z

-- Compling this module with the incomplete-patterns warning will show all the patterns that got missed.
--

-- $ ghc -Wincomplete-patterns Example.hs
-- [1 of 1] Compiling Example          ( Example.hs, Example.o )

-- Example.hs:17:1: warning: [-Wincomplete-patterns]
--     Pattern match(es) are non-exhaustive
--     In an equation for ‘example’:
--         Patterns of type ‘Bool’, ‘[a]’ not matched:
--             True (_:_)
--             False []
--             False [_]
--             False (_:_:_:_)
--    |
-- 17 | example True [] = 0
--    | ^^^^^^^^^^^^^^^^^^^...
example :: (Num a) => Bool -> [a] -> a
example True [] = 0
example False [x, y] = x + y

-- data Color = Red | Green | Blue

-- showColor Red = "This is red"
-- showColor Green = "This is green"
-- showColor Blue = "This is blue"

-- displayColor color = case color of
--   Red -> (255, 0, 0)
--   Green -> (0, 255, 0)
--   Blue -> (0, 0, 255)

data User = User
  { userName :: String,
    userScore :: Int,
    userIsActive :: Bool
  }
  deriving (Show)

aptg =
  User
    { userName = "aptg",
      userScore = 100,
      userIsActive = True
    }

avilay = aptg {userName = "avilay"}

predict user
  | userIsActive user = userScore user * 10
  | otherwise = userScore user `div` 10

tally (User userName userScore userIsActive)
  | userIsActive = userScore + 10
  | otherwise = userScore `div` 10

eval :: Char -> Int -> Int -> Int
eval '+' x y = x + y
eval '-' x y = x - y
eval '*' x y = x * y
eval '/' x y = x `div` y
eval _ _ _ = 0

calc :: Char -> Int -> Int -> Int
calc op x y = case op of
  '+' -> x + y
  '-' -> x - y
  '*' -> x * y
  '/' -> x `div` y
  _ -> 0

data Result = Error String | Ok Int

divide :: Int -> Int -> Result
divide _ 0 = Error "Division by zero!"
divide x y = Ok (x `div` y)

showResult :: Result -> String
showResult (Error msg) = "Error: " ++ msg
showResult (Ok answer) = "Ok: " ++ show answer

displayResult :: Result -> String
displayResult result = case result of
  Error msg -> "Error: " ++ msg
  Ok answer -> "Answer: " ++ show answer

data Property
  = Padding Int
  | Clickable Bool Int
  | Description String

data IntList = Empty | Prepend Int IntList deriving (Show)

intListLength :: IntList -> Int
intListLength Empty = 0
intListLength (Prepend _ xs) = 1 + intListLength xs

testFunc :: Maybe Int -> Int
testFunc Nothing = 0
testFunc (Just n) = n

unbox Nothing = 0
unbox (Just x) = x

data Point = Point {x :: Float, y :: Float}

distance :: Point -> Point -> Float
-- distance p1 p2 =
--   let xIntercept = x p1 - x p2
--       yIntercept = y p1 - y p2
--    in sqrt ((xIntercept ^ 2) + (yIntercept ^ 2))

distance (Point x1 y1) (Point x2 y2) =
  let xIntercept = x1 - x2
      yIntercept = y1 - y2
   in sqrt ((xIntercept ^ 2) + (yIntercept ^ 2))
