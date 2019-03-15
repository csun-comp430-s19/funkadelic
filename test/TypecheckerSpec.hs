{-# LANGUAGE FlexibleContexts #-}
module TypecheckerSpec where
import Parser hiding (type')
import Typechecker
import Test.Hspec

type' :: String -> Type
type' s = Type $ Identifier s


typecheckerSpec :: IO ()
typecheckerSpec = hspec tcSpec

tcSpec = do
    describe "typechecking integer expressions" $ do
        it "typechecks integer expressions" $ do
            typecheck (IExp (IExpInt 1) Plus (IExpInt 1)) `shouldBe` (Just $ type' "Int")
            typecheck (IExpInt 1) `shouldBe` (Just $ type' "Int")


    describe "typechecking expressions" $ do
        it "typechecks Expressions" $ do
            typecheck (ExpInteger 1234) `shouldBe` (Just $ type' "Int")
            typecheck (ExpString "xyz") `shouldBe` (Just $ type' "String")
            typecheck (ExpIExp (IExpInt 1)) `shouldBe` (Just $ type' "Int")
            typecheck (ExpIExp (IExp (IExpInt 1) Plus (IExpInt 1))) `shouldBe` (Just $ type' "Int")
            typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "Int")) `shouldBe` (Just $ type' "Int")
            typecheck (ExpLambda (ExpString "1234") (type' "String") (ExpInteger 1234) (type' "Int")) `shouldBe` (Just $ type' "Int")
            typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpString "1234") (type' "String")) `shouldBe` (Just $ type' "String")
           

    -- describe "integration integer expressions" $ do
    --     it "tests integration of typecheck IExpressions and parsing IExpressions" $ do
    --         typecheck (getRight (parseIExp "1+1")) `shouldBe` (Just $ type' "Int")
    --         typecheck (getRight (parseIExp "1")) `shouldBe` (Just $ type' "Int")


    -- describe "typechecking expressions" $ do
    --     it "tests integration of typecheck expressions and parsing expressions" $ do
    --         typecheck (getRight (parseExp "1234")) `shouldBe` (Just $ type' "Int")
    --         typecheck (getRight (parseExp "\"xyz\"")) `shouldBe` (Just $ type' "String")
    --         typecheck (getRight (parseExp "1+1")) `shouldBe` (Just $ type' "Int")