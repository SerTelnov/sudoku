module Test.EventHandlerSpec
  ( spec
  ) where

import  Control.Lens.Getter
import  Data.List                (transpose)
import  Test.Hspec               (Spec, describe, it, shouldBe)

import  qualified Data.Map.Strict as Map

import  Common
import  EventHandler
import  Sudoku
import  Generator.GeneratorUtil

testEnv :: GameEnv
testEnv = let field     = [[1, 2], [2, 1]]
              gameField = [[Opened 1, Closed], [Opened 2, Closed]]
          in makeGameEnv field gameField Easy (GeneratorEnv [] [])

spec :: Spec
spec = do
    describe "Open cell test" $ do
      it "Corrent value to open" $ do
        let coord         = (1, 0)
        let (Right env)   = openCell testEnv coord 2

        (env ^. numHolder) 
          `shouldBe` 
            Map.fromList
              [ ((0, 0), 1)
              , ((1, 0), 2)
              , ((0, 1), 2) ]
        (env ^. currentGameField)
          `shouldBe`
          [[Opened 1, Opened 2], [Opened 2, Closed]]
      it "Wrong value to open" $ do
        let eventResult = openCell testEnv (1, 0) 3
        eventResult `shouldBe` (Left WrongValue)
      it "Already opened cell" $ do
        let eventResult = openCell testEnv (0, 0) 1
        eventResult `shouldBe` (Left AlreadyOpen)
