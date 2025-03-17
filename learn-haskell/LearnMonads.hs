module LearnMonads where

import Data.Maybe (isNothing)
import Example (Point (x))

maybePlusSimple :: Maybe Int -> Maybe Int -> Maybe Int
maybePlusSimple Nothing _ = Nothing
maybePlusSimple _ Nothing = Nothing
maybePlusSimple (Just x) (Just y) = Just (x + y)

eitherPlusSimple :: Either String Int -> Either String Int -> Either String Int
eitherPlusSimple (Left err) _ = Left err
eitherPlusSimple _ (Left err) = Left err
eitherPlusSimple (Right x) (Right y) = Right (x + y)

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: ma -> (a -> m b) -> m b

-- instance Monad Maybe where
--   return x = Just x
--   (>>=) Nothing _ = Nothing
--   (>>=) (Just x) f = f x

-- Tries to unbox the container, if it has something in it, apply the given
-- function to the value contained in it, otherwise just return the empty container.
-- The given function is usually another thenMaybe whose first argument is another
-- container. If this second container has something in it, then apply the given
-- function to it and the first value which is passed in through closure. This
-- second given function adds the two values.
thenMaybe :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
thenMaybe Nothing _ = Nothing
thenMaybe (Just x) f = f x

thenEither :: Either String Int -> (Int -> Either String Int) -> Either String Int
thenEither (Left err) _ = Left err
thenEither (Right x) f = f x

maybePlus :: Maybe Int -> Maybe Int -> Maybe Int
-- maybePlus maybex maybey = thenMaybe maybex (\x -> thenMaybe maybey (\y -> Just (x + y)))
maybePlus maybex maybey = thenMaybe maybex lambda1
  where
    lambda1 x = thenMaybe maybey lambda2
      where
        lambda2 y = Just (x + y)

maybePlusMonad :: Maybe Int -> Maybe Int -> Maybe Int
-- maybePlusMonad maybex maybey = (>>=) maybex (\x -> (>>=) maybey (\y -> Just (x + y)))
maybePlusMonad maybex maybey = maybex >>= (\x -> maybey >>= (\y -> Just (x + y)))

eitherPlus :: Either String Int -> Either String Int -> Either String Int
-- eitherPlus eitherx eithery = thenEither eitherx (\x -> thenEither eithery (\y -> Right (x + y)))
eitherPlus eitherx eithery = thenEither eitherx lambda1
  where
    lambda1 x = thenEither eithery lambda2
      where
        lambda2 y = Right (x + y)

testMaybePlus :: (Maybe Int -> Maybe Int -> Maybe Int) -> Bool
testMaybePlus testFunc =
  let ans1 = testFunc (Just 10) (Just 20)
      ans2 = testFunc Nothing (Just 20)
      ans3 = testFunc (Just 20) Nothing
      ans4 = testFunc Nothing Nothing
      test1 = ans1 == Just 30
      test2 = isNothing ans2
      test3 = isNothing ans3
      test4 = isNothing ans4
   in test1 && test2 && test3 && test4

testEitherPlus :: (Either String Int -> Either String Int -> Either String Int) -> Bool
testEitherPlus testFunc =
  let ans1 = testFunc (Right 10) (Right 20)
      ans2 = testFunc (Left "error!!") (Right 20)
      ans3 = testFunc (Right 10) (Left "error!!")
      ans4 = testFunc (Left "error!!") (Left "error!!")
      test1 = ans1 == Right 30
      test2 = ans2 == Left "error!!"
      test3 = ans3 == Left "error!!"
      test4 = ans4 == Left "error!!"
   in test1 && test2 && test3 && test4
