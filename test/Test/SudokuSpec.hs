module Test.SudokuSpec
  ( spec
  ) where

import  Test.Hspec  (Spec, describe, it, shouldBe)
import  Sudoku
import  Common
import  Control.Lens.Getter
import  Generator.GeneratorUtil

import  qualified Data.Map.Strict as Map

testGameEnv :: Field -> GameField -> GameEnv
testGameEnv field gameField = makeGameEnv field gameField Easy (GeneratorEnv [] [])

spec :: Spec
spec = do
    describe "Generate number holder" $ do
     it "Simple test" $ do
      let testField = [[1, 2], [3, 4]]
          gameField = [[Opened 1, Opened 2], [Opened 3, Opened 4]]
          env       = testGameEnv testField gameField
      (env ^. numHolder) 
        `shouldBe` 
          Map.fromList
            [ ((0, 0), 1)
            , ((1, 0), 2)
            , ((0, 1), 3)
            , ((1, 1), 4) ]
    it "All cells opened test" $ do
      let testField = [[1, 2, 3], [3, 2, 1]]
          gameField = [[Opened 1, Opened 2, Opened 3], [Opened 3, Opened 2, Opened 1]]      
          env       = testGameEnv testField gameField
      (env ^. numHolder)
        `shouldBe`
          Map.fromList
            [ ((0, 0), 1)
            , ((1, 0), 2)
            , ((2, 0), 3)
            , ((0, 1), 3)
            , ((1, 1), 2)
            , ((2, 1), 1)]
    it "Some cells cloned test" $ do
      let testField = [[1, 2, 3], [3, 2, 1]]
          gameField = [[Opened 1, Closed, Opened 3], [Closed, Opened 2, Opened 1]]      
          env       = testGameEnv testField gameField
      (env ^. numHolder)
        `shouldBe`
          Map.fromList
            [ ((0, 0), 1)
            , ((2, 0), 3)
            , ((1, 1), 2)
            , ((2, 1), 1)]
