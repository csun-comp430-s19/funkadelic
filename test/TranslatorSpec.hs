{-# LANGUAGE FlexibleContexts #-}
module TranslatorSpec where
import Parser hiding (type')
import Translator
import Test.Hspec

trSpec = do
    describe "integer expressions" $ do
        it "translates integer expression into javascript" $ do
            translate (IExpInt 1) `shouldBe` "1"
            translate (IExpVar $ Identifier "x") `shouldBe` "x"
            translate (IExp (IExpInt 1) Plus (IExpInt 1)) `shouldBe` "1 + 1"
            translate (IExp (IExpVar $ Identifier "x") Minus (IExpInt 2)) `shouldBe` "x - 2"
            translate (IExp (IExpInt 4) Mult (IExpVar $ Identifier "x")) `shouldBe` "4 * x"
            translate (IExp (IExpVar $ Identifier "x") Div (IExpVar $ Identifier "y")) `shouldBe` "x / y"
