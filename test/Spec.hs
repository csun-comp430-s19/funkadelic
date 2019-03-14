{-# LANGUAGE FlexibleContexts #-}
import Parser hiding (type')
import Typechecker
import Test.Hspec

main :: IO ()
main = hspec spec

type' :: String -> Type
type' s = Type $ Identifier s

getRight :: Either a b -> Maybe b
getRight y = do 
    Right x <- return y
    return x

parseIExp input = parse' iExpParser input
parseExp input = parse' expParser input
parseTld input = parse' tldParser input

spec = do
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

    describe "expressions" $ do
        it "parses expressions" $ do
            parseExp "x" `shouldBe` (Right $ ExpVariable $ Identifier "x")
            parseExp "x23232" `shouldBe` (Right $ ExpVariable $ Identifier "x23232")
            parseExp "12323232" `shouldBe` (Right $ ExpInteger 12323232)
            parseExp "\"xyz\"" `shouldBe` (Right $ ExpString "xyz")
            parseExp "\\(x):String{x}:String" `shouldBe` (Right (ExpLambda (ExpVariable $ Identifier "x") (Type $ Identifier "String") (ExpVariable $ Identifier "x") (Type $ Identifier "String")))
            parseExp "x*y" `shouldBe` (Right $ ExpIExp $ IExp (IExpVar $ Identifier "x") Mult (IExpVar $ Identifier "y"))
            parseExp "name(x)" `shouldBe` (Right (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x")))
            parseExp "name()" `shouldBe` (Right (ExpNullaryFOCall (Identifier "name")))

    describe "top level function and data definitions" $ do
        it "parses tlds" $ do
            parseTld "datanewType=Nullary()" `shouldBe` (Right (DataDef (Identifier "newType") [NullaryConstructor $ Identifier "Nullary"]))
            parseTld "datanewType=Calculate(Integer)" `shouldBe` (Right (DataDef (Identifier "newType") [UnaryConstructor (Identifier "Calculate") (Type $ Identifier "Integer")]))
            parseTld "datanewType=SomeFunct()Calculate(Integer)" `shouldBe` (Right (DataDef (Identifier "newType") [(NullaryConstructor (Identifier "SomeFunct")), (UnaryConstructor (Identifier "Calculate") (Type $ Identifier "Integer"))]))
            parseTld "funk=func(a:string):string{a}" `shouldBe` (Right (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "string") (ExpVariable $ Identifier "a") (Type $ Identifier "string")))
            parseTld "funk=func():string{a}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpVariable $ Identifier "a") (Type $ Identifier "string")))
            parseTld "funk=func():string{x*y}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExpVar (Identifier "y")))) (Type (Identifier "string"))))
            parseTld "funk=func():string{x*y+x==5}" `shouldBe` (Right (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExp (IExpVar (Identifier "y")) Plus (IExp (IExpVar (Identifier "x")) Equals (IExpInt 5))))) (Type (Identifier "string"))))
    
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