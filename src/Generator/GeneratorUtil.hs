{-# LANGUAGE TemplateHaskell #-}

module Generator.GeneratorUtil
  ( SwapOption (..)
  , GeneratorStep (..)
  , AreaNumber
  , CellIndex

  , GeneratorEnv (..)
  , generatorSteps
  , removeCells
  ) where

import  System.Random            (Random, RandomGen, randomR, random)
import  Control.Lens.Combinators (makeLenses)

type CellIndex = Int

-- | Options to swap two rows (or areas)
data SwapOption
  = FirstAndSecond
  | FirstAndThird
  | SecondAndThird
  deriving (Show, Eq, Enum)

-- | Area of field
-- Field is composed of nine blocks
-- Area is combination of three blocks (horizontally or upright)
type AreaNumber = Int

-- | Ways to change the field
data GeneratorStep
  = Transposing -- ^ Transposing field
  | SwapRows AreaNumber SwapOption -- ^ Swap two rows in single area
  | SwapColumns AreaNumber SwapOption -- ^ Swap two column in single area
  | SwapHorAreas SwapOption -- ^ Swap two areas horizontally
  | SwapUprightAreas SwapOption -- ^ Swap two areas upright
  deriving (Show, Eq)


instance Enum GeneratorStep where
  toEnum :: Int -> GeneratorStep
  toEnum n
    | n < 0  = error "Enum can't be negative"
    | n < 9  = let (a, op) = uncodeRow n in SwapRows a op
    | n < 18 = let (a, op) = uncodeRow (n - 9) in SwapColumns a op
    | n < 21 = let op = uncodeSwapOption (n - 18) in SwapHorAreas op
    | n < 24 = let op = uncodeSwapOption (n - 21) in SwapUprightAreas op
    | otherwise = Transposing
    where
      uncodeRow :: Int -> (AreaNumber, SwapOption)
      uncodeRow i = let areaNumber = i `div` 3
                        swapOption = uncodeSwapOption (i `mod` 3)
                    in (areaNumber, swapOption)

      uncodeSwapOption :: Int -> SwapOption
      uncodeSwapOption 0 = FirstAndSecond
      uncodeSwapOption 1 = FirstAndThird
      uncodeSwapOption 2 = SecondAndThird
      uncodeSwapOption _ = error "Impossible option"

  fromEnum :: GeneratorStep -> Int
  fromEnum (SwapRows n op) = fromEnum op + n * 3
  fromEnum (SwapColumns n op) = fromEnum op + n * 3 + 9
  fromEnum (SwapHorAreas op) = fromEnum op + 18
  fromEnum (SwapUprightAreas op) = fromEnum op + 21
  fromEnum Transposing = 24


instance Random GeneratorStep where
  randomR :: (RandomGen g) => (GeneratorStep, GeneratorStep) -> g -> (GeneratorStep, g)
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random :: (RandomGen g) => g -> (GeneratorStep, g)
  random = randomR (toEnum 0, toEnum 24)
  

-- | Generated value for game
data GeneratorEnv = GeneratorEnv
  { _generatorSteps :: [GeneratorStep]
  , _removeCells :: [CellIndex]
  } deriving (Show, Eq)

makeLenses ''GeneratorEnv
