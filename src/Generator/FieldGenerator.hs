module Generator.FieldGenerator
    ( runFieldGenerator
    
    , GeneratorEnv (..)
    , baseField
    ) where

import  Data.List               (transpose)
import  Common                  (Field)
import  Generator.GeneratorUtil (AreaNumber, SwapOption (..), GeneratorStep (..))
import  Control.Monad.State     (State, evalState, get, put, execState)
import  Control.Monad.Reader    (ReaderT, runReaderT, asks)
import  System.Random           (Random, RandomGen, random, randomR)


-- | Base field
baseField :: Field
baseField = map (\i -> internal i) cellsInRow
  where
    cellsInRow :: [Int]
    cellsInRow = [0..8]

    internal :: Int -> [Int]
    internal i = map (\j -> (i * 3 + (i `div` 3) + j) `mod` 9 + 1) cellsInRow

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
