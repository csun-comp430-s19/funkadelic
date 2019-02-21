{-# LANGUAGE FlexibleContexts #-}
import Parser
import Test.Hspec

main :: IO ()
main = hspec spec

parseIExp input = parse' iExp input
parseExp input = parse' exp' input
parseTld input = parse' tld input

spec = do
    describe "integer expressions" $ do
        it "parses integer expressions" $ do
            parseIExp "1+1" `shouldBe` (Right $ IExp (IExpInt 1) Plus (IExpInt 1))
            parseIExp "x-1" `shouldBe` (Right $ IExp (IExpVar $ Identifier "x") Minus (IExpInt 1))
            parseIExp "x*y" `shouldBe` (Right $ IExp (IExpVar $ Identifier "x") Mult (IExpVar $ Identifier "y"))
            -- "x*y==z" unresolved bug with recursive integer expressions
    
    describe "expressions" $ do
        it "parses expressions" $ do
            parseExp "x" `shouldBe` (Right $ ExpVariable $ Identifier "x")
            parseExp "x23232" `shouldBe` (Right $ ExpVariable $ Identifier "x23232")
            parseExp "12323232" `shouldBe` (Right $ ExpInteger 12323232)
            parseExp "\"xyz\"" `shouldBe` (Right $ ExpString "xyz")
            parseExp "\\(x){x}:String" `shouldBe` (Right (ExpLambda (ExpVariable $ Identifier "x") (ExpVariable $ Identifier "x") (Type $ Identifier "String")))
            parseExp "name(x)" `shouldBe` (Right (ExpFOCall (Identifier "name") (ExpVariable $ Identifier "x")))
    
    describe "top level function and data definitions" $ do
        it "parses tlds" $ do
            parseTld "datanewType=Nullary" `shouldBe` (Right (DataDef (Identifier "newType") [NullaryConstructor $ Identifier "Nullary"]))
            -- unresolved bug with multiple constructors
            parseTld "funk=func(a:string):string{a}" `shouldBe` (Right (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "string") (ExpVariable $ Identifier "a") (Type $ Identifier "string")))
            parseTld "funk=func():string{a}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpVariable $ Identifier "a") (Type $ Identifier "string")))
