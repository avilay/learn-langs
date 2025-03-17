-- typedef
type EntityId = Int

-- newtype
newtype Year = Year Int

newtype Day = Day Int

newtype Stars = Stars Int

-- nullary
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | November
  | December

data Genre
  = ActionAdventure
  | Fighting
  | Shooter
  | Simulation
  | FamilyKids
  | PuzzleTrivia
  | RolePlaying

data Color = Red | Green | Blue

-- sum type
data Result = Error String | Ok Int

-- product type
data Date = Date {year :: Year, month :: Month, day :: Day}

data VideoGame = VideoGame
  { title :: String,
    publishedBy :: String,
    developedBy :: String,
    releaseDate :: Date,
    genre :: Genre,
    rating :: Stars,
    numReviews :: Int,
    price :: Float,
    isOnPC :: Bool,
    description :: String
  }

data User = User
  { userName :: String,
    userScore :: Int,
    userIsActive :: Bool
  }

data Point = Point Float Float

-- combination of sum and product
data Shape = Circle Point Float | Rectangle Point Point

data Property = Padding Int | Clickable Bool Int | Description String

-- recursive data types
data IntList = Empty | Prepend Int IntList

-- parameterized data types
data Maybe a = Nothing | Just a

-- parametric polymorphic functions

-- typeclass for concrete types aka ad-hoc polymorphic functions
class Display a where
  display :: a -> String

--    typeclass with a super type

-- typeclass for type constructors
class Singleton f where
  singleton :: a -> f a
