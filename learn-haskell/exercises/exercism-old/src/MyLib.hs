module MyLib where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T

rotate :: Int -> String -> String
rotate n = go []
    where
        ascii :: Int -> Char -> Int
        ascii offset c =
            let plainOffset = ord c - offset
                cipherOffset = (plainOffset + n) `mod` 26
            in cipherOffset + offset

        go :: [Int] -> String -> String
        go acc [] = map chr acc
        go acc (x : xs)
            | isAsciiLower x = go (acc ++ [ascii (ord 'a') x]) xs
            | isAsciiUpper x = go (acc ++ [ascii (ord 'A') x]) xs
            | otherwise = go (acc ++ [ord x]) xs


score :: Float -> Float -> Int
score x y
    | r <= 1.0 = 10
    | r <= 5.0 = 5
    | r <= 10.0 = 1
    | otherwise = 0
    where
        r = sqrt (x^2 + y^2)


-- responseFor :: Text -> Text
-- responseFor content
--     | T.null phrase = T.pack "Fine. Be that way!"
--     | isQuestion phrase && isYelling phrase = T.pack "Calm down, I know what I'm doing!"
--     | isQuestion phrase = T.pack "Sure."
--     | isYelling phrase = T.pack "Whoa, chill out!"
--     | otherwise = T.pack "Whatever."
--     where
--         phrase = T.strip content

--         isQuestion :: Text -> Bool
--         isQuestion text = go1 (T.unsnoc text)
--             where
--                 go1 :: Maybe (Text, Char) -> Bool
--                 go1 Nothing              = False
--                 go1 (Just (_, lastChar)) = lastChar == '?'

--         isYelling :: Text -> Bool
--         isYelling text = go2 (T.uncons text)
--             where
--                 go2 :: Maybe (Char, Text) -> Bool
--                 go2 Nothing = False
--                 go2 (Just (firstChar, text)) = isUpper firstChar || go2 (T.uncons text)

responseFor :: Text -> Text
responseFor phrase
    | T.null . T.strip $ phrase = T.pack "Fine. Be that way!"
    | isQuestion clean && isYelling clean = T.pack "Calm down, I know what I'm doing!"
    | isQuestion clean = T.pack "Sure."
    | isYelling clean = T.pack "Whoa, chill out!"
    | otherwise = T.pack "Whatever."
    where
        clean = T.filter isLetter phrase

        isQuestion :: Text -> Bool
        isQuestion text = go (T.unsnoc text)
            where
                go :: Maybe (Text, Char) -> Bool
                go Nothing              = False
                go (Just (_, lastChar)) = lastChar == '?'

        isYelling :: Text -> Bool
        isYelling text = T.toUpper text == text
