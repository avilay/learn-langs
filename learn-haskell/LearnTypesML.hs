module LearnTypesML where

-- Concrete data types
-- Enum type
data Quantization = INT8 | FP16 | BF16 | FP32 deriving (Show)

-- Product type
data HyperParams = HyperParams
  { learningRate :: Float,
    numEpochs :: Int,
    computeMetrics :: Bool,
    quantization :: Quantization
  }
  deriving (Show)

-- Sum type
data JobStatus = Scheduled | Running Int | Terminated String deriving (Show)

-- newtype
newtype FSDP = FSDP String deriving (Show)

newtype DDP = DDP String deriving (Show)

-- Typeclasses define a group of functions that behave differently for different concrete data types
-- FSDP will implement train and checkpoint differently from DDP and so on.
class Trainer a where
  -- Given a training strategy, this interface aka typeclass will train and checkpoint
  train :: a -> HyperParams -> JobStatus
  checkpoint :: a -> String

instance Trainer FSDP where
  train fsdp hparams = Running 5
  checkpoint fsdp = "Checkpointing FSDP"

instance Trainer DDP where
  train ddp hparams = Running 3
  checkpoint ddp = "Checkpointing DDP"

-- Functions constrained by this typeclass
buildLLM :: (Trainer a) => a -> String
buildLLM strategy =
  let hparams = HyperParams {learningRate = 0.02, numEpochs = 1, computeMetrics = False, quantization = BF16}
      job = train strategy hparams
      modelLocation = checkpoint strategy
   in modelLocation ++ " has been saved!"

-- Polymorphic functions behave the same for any data type
partiallyReverse :: [a] -> [a]
partiallyReverse list =
  let mid = length list `div` 2
      (left, right) = splitAt mid list
   in left ++ reverse right

-- Parameteric data types are like generics in other languages.
-- They usually "contain" some other data type which is left unspecified
