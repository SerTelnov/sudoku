module Test.Generator.GameGeneratorSpec
  ( spec
  ) where

import  Test.Hspec               (Spec, describe, it, shouldBe)
import  Generator.FieldGenerator (baseField)
import  Generator.GameGenerator  (generateGameField)
import  Common                   (Difficulties ( Easy ))

spec :: Spec
spec = do
    describe "Generate game test" $ do
     it "remove first row" $ do
      show (generateGameField baseField [1..9] Easy) `shouldBe` 
        "[[.,.,.,.,.,.,.,.,.]" ++
        ",[4,5,6,7,8,9,1,2,3]" ++
        ",[7,8,9,1,2,3,4,5,6]" ++
        ",[2,3,4,5,6,7,8,9,1]" ++ 
        ",[5,6,7,8,9,1,2,3,4]" ++
        ",[8,9,1,2,3,4,5,6,7]" ++
        ",[3,4,5,6,7,8,9,1,2]" ++
        ",[6,7,8,9,1,2,3,4,5]" ++
        ",[9,1,2,3,4,5,6,7,8]]"
