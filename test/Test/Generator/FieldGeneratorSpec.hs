module Test.Generator.FieldGeneratorSpec
  ( spec
  ) where

import  Data.List                (transpose)
import  Test.Hspec               (Spec, describe, it, shouldBe)
import  Generator.FieldGenerator
import  Generator.GeneratorUtil


spec :: Spec
spec = do
    describe "Generate field test" $ do
      it "Generate" $ do
        runFieldGenerator [] baseField `shouldBe` baseField
      it "Swap Rows and Column with changes" $ do
        runFieldGenerator 
          [ SwapRows 0 FirstAndSecond
          , SwapColumns 1 FirstAndThird
          , SwapColumns 2 SecondAndThird
          , SwapRows 1 SecondAndThird ]
          baseField
        `shouldBe`
          [ [4,5,6,9,8,7,1,3,2]
          , [1,2,3,6,5,4,7,9,8]
          , [7,8,9,3,2,1,4,6,5]
          , [2,3,4,7,6,5,8,1,9]
          , [8,9,1,4,3,2,5,7,6]
          , [5,6,7,1,9,8,2,4,3]
          , [3,4,5,8,7,6,9,2,1]
          , [6,7,8,2,1,9,3,5,4]
          , [9,1,2,5,4,3,6,8,7]]
      it "Swap areas" $ do
        runFieldGenerator
          [ SwapHorAreas FirstAndSecond
          , SwapHorAreas SecondAndThird
          , SwapUprightAreas FirstAndThird]
          baseField
        `shouldBe`
          [ [8,9,1,5,6,7,2,3,4]
          , [2,3,4,8,9,1,5,6,7]
          , [5,6,7,2,3,4,8,9,1]
          , [9,1,2,6,7,8,3,4,5]
          , [3,4,5,9,1,2,6,7,8]
          , [6,7,8,3,4,5,9,1,2]
          , [7,8,9,4,5,6,1,2,3]
          , [1,2,3,7,8,9,4,5,6]
          , [4,5,6,1,2,3,7,8,9]]
      it "Transposing change" $ do
        runFieldGenerator [Transposing] baseField
          `shouldBe` (transpose baseField)
      it "Minor Transposing changes" $ do
        runFieldGenerator
          [ Transposing
          , Transposing]
          baseField
        `shouldBe` baseField
