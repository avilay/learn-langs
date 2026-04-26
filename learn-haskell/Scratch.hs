module Scratch where

foo :: Num a => a -> a
foo i = 10 * i

usingLet :: Num a => a -> a
usingLet x = do
  let y = 10
      z = foo y
  x + y - z