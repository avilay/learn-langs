import Text.XHtml.Transitional (black)

{-
Consider a function `safetail` that behaves in the same way as tail, except that safetail maps the empty list to the
empty list, whereas tail gives an error in this case. Define safetail using:
  - a conditional expression
  - guarded equations
  - pattern matching
-}
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
  | null xs = []
  | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_ : xs) = xs

{-
Give three possible definitions for the logical or operator (||) using pattern matching
-}
True || True = True
True || False = True
False || True = True
False || False = False

or True _ = True
or _ True = True
or _ _ = False

or2 False b = b
or2 True _ = True
