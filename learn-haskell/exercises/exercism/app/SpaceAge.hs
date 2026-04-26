module SpaceAge where

import Test.HUnit

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

-- toPlanetYears :: Planet -> Float -> Float
-- toPlanetYears planet earthYears = case planet of
--   Mercury -> earthYears / 0.2408467
--   Venus -> earthYears / 0.61519726
--   Earth -> earthYears
--   Mars -> earthYears / 1.8808158
--   Jupiter -> earthYears / 11.862615
--   Saturn -> earthYears / 29.447498
--   Uranus -> earthYears / 84.016846
--   Neptune -> earthYears / 164.79132

-- toEarthYears :: Float -> Float
-- toEarthYears ageInSeconds = ageInSeconds / 31557600

-- ageOn :: Planet -> Float -> Float
-- ageOn planet ageInSeconds = toPlanetYears planet . toEarthYears $ ageInSeconds

ageOn :: Planet -> Float -> Float
ageOn Earth seconds = seconds / 31557600
ageOn Mercury seconds = ageOn Earth seconds / 0.2408467
ageOn Venus seconds = ageOn Earth seconds / 0.61519726
ageOn Mars seconds = ageOn Earth seconds / 1.8808158
ageOn Jupiter seconds = ageOn Earth seconds / 11.862615
ageOn Saturn seconds = ageOn Earth seconds / 29.447498
ageOn Uranus seconds = ageOn Earth seconds / 84.016846
ageOn Neptune seconds = ageOn Earth seconds / 164.79132

-- Test cases
testAgeOnMercury :: Test
testAgeOnMercury = TestCase (assertBool "Float mismatch" (abs (4.15201 - (ageOn Mercury 31557600)) < 1e-3))

-- Test suite
tests :: Test
tests = TestList
    [ 
      TestLabel "mercury test" testAgeOnMercury
    ]

-- Main function to run all tests
runTests :: IO ()
runTests = do
    _ <- runTestTT tests
    return ()
