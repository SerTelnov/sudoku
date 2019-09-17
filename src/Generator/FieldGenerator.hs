module Generator.FieldGenerator
    ( runFieldGenerator
    , GeneratorStep (..)

    , SwapOption (..)
    , GeneratorEnv (..)
    , baseField
    ) where

import  Data.List            (transpose)
import  Common               (Field)
import  Control.Monad.State  (State, evalState, get, put, execState)
import  Control.Monad.Reader (ReaderT, runReaderT, asks)
import  System.Random        (Random, RandomGen, random, randomR)


-- | Base field
baseField :: Field
baseField = map (\i -> internal i) cellsInRow
  where
    cellsInRow :: [Int]
    cellsInRow = [0..8]

    internal :: Int -> [Int]
    internal i = map (\j -> (i * 3 + (i `div` 3) + j) `mod` 9 + 1) cellsInRow

-- | Area of field
-- Field is composed of nine blocks
-- Area is combination of three blocks (horizontally or upright)
type AreaNumber = Int

-- | Options to swap two rows (or areas)
data SwapOption
  = FirstAndSecond
  | FirstAndThird
  | SecondAndThird
  deriving (Show, Eq, Enum)

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


data GeneratorEnv = GeneratorEnv
  { steps :: [GeneratorStep]
  }

type FieldGenerator = ReaderT GeneratorEnv (State Field) Field

runFieldGenerator :: [GeneratorStep] -> Field -> Field 
runFieldGenerator genSteps startField =
  let env = GeneratorEnv genSteps
  in evalState (runReaderT generate env) startField

generate :: FieldGenerator
generate = do
  genSteps <- asks steps
  currentField <- get
  return $ execState (mapM internal genSteps) currentField
  where
    internal :: GeneratorStep -> State Field ()
    internal step = do
      fld <- get

      let newFld = case step of Transposing         -> transpose fld
                                SwapRows a op       -> swapRows fld a op
                                SwapColumns a op    -> swapColumns fld a op
                                SwapHorAreas op     -> swapHorAreas fld op
                                SwapUprightAreas op -> swapUprightAreas fld op

      put newFld
      return ()

swapRows :: Field -> AreaNumber -> SwapOption -> Field
swapRows (a:b:c:rest) 0 op = case op of
                               FirstAndSecond -> b : a : c : rest
                               FirstAndThird  -> c : b : a : rest
                               SecondAndThird -> a : c : b : rest
swapRows (a:b:c:rest) n op = a : b : c : (swapRows rest (n - 1) op)
swapRows _ _ _             = error "Impossible option"

swapColumns :: Field -> AreaNumber -> SwapOption -> Field
swapColumns field n op = transpose $ swapRows (transpose field) n op

swapHorAreas :: Field -> SwapOption -> Field
swapHorAreas field op = case op of
                          FirstAndSecond -> second ++ first ++ third
                          FirstAndThird  -> third ++ second ++ first
                          SecondAndThird -> first ++ third ++ second
  where
    first  = take 3 field
    second = drop 3 $ take 6 field
    third  = drop 6 field

swapUprightAreas :: Field -> SwapOption -> Field
swapUprightAreas field op = transpose $ swapHorAreas (transpose field) op
