module Lecture2Weekday where

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq, Enum, Bounded)

toShortString :: Weekday -> String
toShortString Monday = "Mon"
toShortString Tuesday = "Tue"
toShortString Wednesday = "Wed"
toShortString Thursday = "Thu"
toShortString Friday = "Fri"
toShortString Saturday = "Sat"
toShortString Sunday = "Sun"

-- next :: Weekday -> Weekday
-- next weekday = toEnum nextVal
--   where
--     maxVal = fromEnum (maxBound @Weekday) + 1
--     nextVal = (fromEnum weekday + 1) `mod` maxVal

next :: (Enum a, Bounded a) => a -> a
next e = toEnum nextVal
  where
    -- this function is just so I can pull out maxBound for any given type a
    getMaxValue :: (Bounded a) => a -> a
    getMaxValue _ = maxBound

    maxVal = fromEnum (getMaxValue e) + 1
    nextVal = (fromEnum e + 1) `mod` maxVal

testNext :: Bool
testNext =
  let test1 = next Monday == Tuesday
      test2 = next Sunday == Monday
   in test1 && test2

-- daysTo :: Weekday -> Weekday -> Int
-- daysTo fromWeekday toWeekday
--   | fromDay <= toDay = toDay - fromDay
--   | otherwise = (maxDay - fromDay) + (toDay - minDay) + 1
--   where
--     fromDay = fromEnum fromWeekday
--     toDay = fromEnum toWeekday
--     maxDay = fromEnum (maxBound @Weekday)
--     minDay = fromEnum (minBound @Weekday)

daysTo :: (Enum a, Bounded a) => a -> a -> Int
daysTo e0 e1
  | v0 <= v1 = v1 - v0
  | otherwise = (maxv - v0) + (v1 - minv) + 1
  where
    getMaxVal :: (Bounded a) => a -> a
    getMaxVal _ = maxBound

    getMinVal :: (Bounded a) => a -> a
    getMinVal _ = minBound

    v0 = fromEnum e0
    v1 = fromEnum e1
    maxv = fromEnum (getMaxVal e0)
    minv = fromEnum (getMinVal e0)

textDaysTo :: Bool
textDaysTo =
  let test1 = daysTo Monday Tuesday == 1
      test2 = daysTo Friday Wednesday == 5
   in test1 && test2

-- Weekday is kind of concrete so it cannot be an instance of Foldable.
