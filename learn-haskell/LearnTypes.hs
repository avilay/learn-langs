module LearnTypes where

-- Concrete data types
-- Enum type
data Quantization = INT8 | FP16 | BF16 | FP32 deriving (Show)

-- newtype which is the same-name ctor with just a single field
newtype Model = Model String

predict :: Quantization -> Model -> String
predict INT8 (Model model) = "Running prediction on " ++ model ++ " in INT8."
predict FP16 (Model model) = "Running prediction on " ++ model ++ " in FP16."
predict _ _ = "Cannot run prediction on any quantization other than INT8 and FP16."

main1 = predict q m
  where
    q = INT8
    m = Model "Transformer"

-- Product type
data HyperParams = HyperParams
  { learningRate :: Float,
    numEpochs :: Int,
    computeTrainMetrics :: Bool,
    quantization :: Quantization
  }
  deriving (Show)

finetune :: HyperParams -> String
finetune hparams =
  let lr = learningRate hparams
      epochs = numEpochs hparams
   in "Finetuning with learning rate " ++ show lr ++ " for " ++ show epochs ++ " epochs."

main2 = finetune hp
  where
    hp =
      HyperParams
        { learningRate = 0.01,
          numEpochs = 1,
          computeTrainMetrics = False,
          quantization = BF16
        }

-- Sum type
data Result = Error String | Ok Int

computeOne :: Int -> Int -> Result
computeOne x y =
  let z = x + y
   in if z > 10
        then Ok 200
        else Error "Cannot compute very big numbers!"

computeTwo :: Int -> Result -> Int
computeTwo x (Ok code) = if code == 200 then 2 * x else x `div` 2
computeTwo x (Error msg) = -1

-- Sum-of-products general data type
data Feature = IdList [Int] | Dense [Float] | OneHot Int

-- Recursive concrete types
data SetOfInts = Empty | AddToSetOfInts Int SetOfInts deriving (Show)

main3 = AddToSetOfInts 3 (AddToSetOfInts 2 (AddToSetOfInts 1 Empty))

-- Polymorphic functions: Behaves the same way irrespective of the data type
-- This is often called parametric polymorphism.
partialReverse :: [a] -> [a]
partialReverse list =
  let mid = length list `div` 2
      (left, right) = splitAt mid list
   in left ++ reverse right

-- Typeclasses: Group of related functions that behave differently with different data types
-- This is often called ad-hoc polymorphism.
class Trainable a where
  train :: a -> Model -> HyperParams -> String

newtype FSDP = FSDP {numShards :: Int}

newtype DDP = DDP {numReplicas :: Int}

instance Trainable FSDP where
  train (FSDP numShards) (Model model) hparams = "Training " ++ model ++ " using FSDP strategy with " ++ show numShards ++ " for " ++ show (numEpochs hparams) ++ " epochs."

instance Trainable DDP where
  train ddp (Model model) hparams = "Num replicas=" ++ show (numReplicas ddp) ++ " Model=" ++ model ++ " NumEpochs=" ++ show (numEpochs hparams)

-- Typeclasses generally show up as constraints or upper bounds on the input type to a function
-- here my function is going to call train with strategy as input so strategy better be in an instance
-- of Trainable.
buildModel :: (Trainable a) => a -> HyperParams -> Model
buildModel strategy hparams =
  let msg = train strategy (Model "transformer") hparams
   in Model msg

-- What can be confusing between parametric and ad-hoc polymorphism is that in both
-- cases the function/method contains the type parameter a (see partialReverse and train).
-- The difference is that partialReverse is implemented once and that is it.
-- train can be implemented by different concrete types to exhibit different behaviors.

-- Parameteric data types are similar to generics in other languages
-- These are usually types that "contain" some other yet to be specified type.
data Set a = EmptySet | AddToSet a (Set a) deriving (Show)

-- This function is our usual parametric polymorphic function that behaves the
-- same for any data type contained in Set.
contains :: (Eq a) => a -> Set a -> Bool
contains x EmptySet = False
contains x (AddToSet y set) = (x == y) || contains x set

main4 = [contains x set | x <- [3, 2, 1, 10]]
  where
    set = AddToSet 3 (AddToSet 2 (AddToSet 1 EmptySet))

class Uniqueness a where
  isUnique :: a -> Bool

instance Uniqueness (Set a) where
  isUnique set = True