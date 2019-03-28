{-# LANGUAGE FlexibleContexts #-}
module ParserSpec where

import Parser hiding (type')
import Test.Hspec

parserSpec :: IO ()
parserSpec = hspec pSpec

getRight :: Either a b -> Maybe b
getRight y = do 
    Right x <- return y
    return x

parseIExp input = parse' iExpParser input
parseExp input = parse' expParser input
parseTld input = parse' tldParser input


pSpec = do
    describe "integer expressions" $ do
        it "parses integer expressions" $ do
            parseIExp "1+1" `shouldBe` (Right $ IExp (IExpInt 1) Plus (IExpInt 1))
            parseIExp "x-1" `shouldBe` (Right $ IExp (IExpVar $ Identifier "x") Minus (IExpInt 1))
            parseIExp "x*y" `shouldBe` (Right $ IExp (IExpVar $ Identifier "x") Mult (IExpVar $ Identifier "y"))
            parseIExp "x/1" `shouldBe` (Right $ IExp (IExpVar $ Identifier "x") Div (IExpInt 1))
            parseIExp "x" `shouldBe` (Right $ IExpVar $ Identifier "x")
            parseIExp "1" `shouldBe` (Right $ IExpInt 1)
            parseIExp "2==y" `shouldBe` (Right $ IExp (IExpInt 2) Equals (IExpVar $ Identifier "y"))
            parseIExp "2^y" `shouldBe` (Right $ IExp (IExpInt 2) Exponent (IExpVar $ Identifier "y"))
            parseIExp "2^y*3-4" `shouldBe` (Right (IExp (IExpInt 2) Exponent (IExp (IExpVar $ Identifier "y") Mult (IExp (IExpInt 3) Minus (IExpInt 4)))))
            parseIExp "2*x+y" `shouldBe` (Right (IExp (IExpInt 2) Mult (IExp (IExpVar $ Identifier "x") Plus (IExpVar $ Identifier "y"))))
            parseIExp "x*y==z" `shouldBe` (Right (IExp (IExpVar $ Identifier "x") Mult (IExp (IExpVar $ Identifier "y") Equals (IExpVar $ Identifier "z"))))

        it "does not parse invalid integer expressions" $ do
            (getRight $ parseIExp "2*_") `shouldBe` Nothing
            (getRight $ parseIExp "2*8a") `shouldBe` Nothing
            (getRight $ parseIExp "3^+2") `shouldBe` Nothing
            (getRight $ parseIExp "3^") `shouldBe` Nothing
            (getRight $ parseIExp "x23232 + _2") `shouldBe` Nothing
            (getRight $ parseIExp "_2+(2)") `shouldBe` Nothing
            (getRight $ parseIExp "") `shouldBe` Nothing
            (getRight $ parseIExp "\\(x):String{x}:String") `shouldBe` Nothing


    describe "expressions" $ do
        it "parses expressions" $ do
            parseExp "x" `shouldBe` (Right $ ExpVariable $ Identifier "x")
            parseExp "x23232" `shouldBe` (Right $ ExpVariable $ Identifier "x23232")
            parseExp "12323232" `shouldBe` (Right $ ExpInteger 12323232)
            parseExp "\"xyz\"" `shouldBe` (Right $ ExpString "xyz")
            parseExp "\\(x):String{x}:String" `shouldBe` (Right (ExpLambda (ExpVariable $ Identifier "x") (Type $ Identifier "String") (ExpVariable $ Identifier "x") (Type $ Identifier "String")))
            parseExp "\\(1):Integer{1}:Integer" `shouldBe` (Right (ExpLambda (ExpInteger 1) (Type $ Identifier "Integer") (ExpInteger 1) (Type $ Identifier "Integer")))
            parseExp "x*y" `shouldBe` (Right $ ExpIExp $ IExp (IExpVar $ Identifier "x") Mult (IExpVar $ Identifier "y"))
            parseExp "name(x)" `shouldBe` (Right (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x")))
            parseExp "name()" `shouldBe` (Right (ExpNullaryFOCall (Identifier "name")))

        it "does not parse invalid expressions" $ do
            (getRight $ parseExp "2a") `shouldBe` Nothing
            (getRight $ parseExp "x23232 + _2") `shouldBe` Nothing
            (getRight $ parseExp "\"xyz\" + 5") `shouldBe` Nothing
            (getRight $ parseExp "") `shouldBe` Nothing
            (getRight $ parseExp "\\(x):String{x}:") `shouldBe` Nothing
            (getRight $ parseExp "name(x") `shouldBe` Nothing
            (getRight $ parseExp "name( )") `shouldBe` Nothing
            (getRight $ parseExp "\"xyz") `shouldBe` Nothing


    describe "top level function and data definitions" $ do
        it "parses tlds" $ do
            parseTld "datanewType=Nullary()" `shouldBe` (Right (DataDef (Identifier "newType") [NullaryConstructor $ Identifier "Nullary"]))
            parseTld "datanewType=Calculate(Integer)" `shouldBe` (Right (DataDef (Identifier "newType") [UnaryConstructor (Identifier "Calculate") (Type $ Identifier "Integer")]))
            parseTld "datanewType=SomeFunct()Calculate(Integer)" `shouldBe` (Right (DataDef (Identifier "newType") [(NullaryConstructor (Identifier "SomeFunct")), (UnaryConstructor (Identifier "Calculate") (Type $ Identifier "Integer"))]))
            parseTld "funk=func(a:string):string{a}" `shouldBe` (Right (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "string") (ExpVariable $ Identifier "a") (Type $ Identifier "string")))
            parseTld "funk=func():string{a}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpVariable $ Identifier "a") (Type $ Identifier "string")))
            parseTld "funk=func():string{x*y}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExpVar (Identifier "y")))) (Type (Identifier "string"))))
            parseTld "funk=func():string{x*y+x==5}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExp (IExpVar (Identifier "y")) Plus (IExp (IExpVar (Identifier "x")) Equals (IExpInt 5))))) (Type (Identifier "string"))))
    
        it "does not parse invalid tlds/data definitions" $ do
            (getRight $ parseTld "datanewType=Nullary(") `shouldBe` Nothing
            (getRight $ parseTld "datanewType=Calculate") `shouldBe` Nothing
            (getRight $ parseTld "funk=func():string{}") `shouldBe` Nothing
            (getRight $ parseTld "funk=func():{}") `shouldBe` Nothing
            (getRight $ parseTld "funk=func():string{x*_}") `shouldBe` Nothing