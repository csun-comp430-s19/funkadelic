{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Lib.Internal
import Test.Hspec.Expectations.Match
import Control.Monad.State.Lazy
import Parser hiding (type')
import Typechecker hiding (main)
import Test.Hspec
import ParserSpec
import TypecheckerSpec

main :: IO ()
main = hspec spec

type' :: String -> Type
type' s = Type $ Identifier s

getRight :: Either a b -> Maybe b
getRight y = do 
    Right x <- return y
    return x

intType = (Just $ type' "Int")
stringType = (Just $ type' "String")

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
            -- parseIExp "2*_" `shouldBe` (Left (ParseError (Message "failed") (SourcePos (SourceName "") (Line 1) (Column 1))))

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
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (evalState (typecheck (IExpVar (Identifier "x"))) typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            (evalState (typecheck (IExpVar (Identifier "y"))) typeEnv) `shouldBe` stringType

            (evalState (typecheck (IExp (IExpInt 1) Plus (IExpInt 1))) typeEnv) `shouldBe` intType
            (evalState (typecheck (IExpInt 1)) typeEnv) `shouldBe` intType


    describe "typechecking expressions" $ do
        it "typechecks Expressions" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (evalState (typecheck (ExpInteger 1234)) typeEnv) `shouldBe` intType

            (evalState (typecheck (ExpString "xyz")) typeEnv) `shouldBe` stringType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "y")))) typeEnv) `shouldBe` stringType

            (evalState (typecheck (ExpIExp (IExpVar (Identifier "z")))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpIExp (IExpInt 1))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpIExp (IExp (IExpInt 1) Plus (IExpInt 1)))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "Int"))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpString "1234") (type' "String") (ExpInteger 1234) (type' "Int"))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpString "1234") (type' "String"))) typeEnv) `shouldBe` stringType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "String"))) typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "Int" "String"))])
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` stringType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "String" "String"))])
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String")])
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", mkType "Int")])
            (evalState (typecheck (ExpNullaryFOCall (Identifier "name"))) typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (evalState (typecheck (ExpNullaryFOCall (Identifier "name"))) typeEnv) `shouldBe` Nothing


    describe "typechecking tlds" $ do
        it "typechecks top level definitions" $ do
            let typeEnv = (Gamma [(Identifier "funk", mkType "String")])
            (evalState (typecheck (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "String") (ExpVariable $ Identifier "a") (Type $ Identifier "String"))) typeEnv) 
                `shouldBe` Just (FunctionType (Type (Identifier "String")) (Type (Identifier "String")))

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (evalState (typecheck (FuncDefNullary (Identifier "anotherFunk") (ExpInteger 1234) (Type $ Identifier "Int"))) typeEnv)
                `shouldBe` Just (Type (Identifier "Int"))

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (evalState (typecheck (FuncDefNullary (Identifier "anotherFunk") (ExpInteger 1234) (Type $ Identifier "String"))) typeEnv) 
                `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (evalState (typecheck (FuncDefNullary (Identifier "anotherFunk") (ExpVariable $ Identifier "a") (Type $ Identifier "String"))) typeEnv) 
                `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (evalState (typecheck (FuncDefNullary (Identifier "anotherFunk") (ExpVariable $ Identifier "a") (Type $ Identifier "String"))) typeEnv) 
                `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "function", mkType "String")])
            (evalState (typecheck (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExp (IExpVar (Identifier "y")) Plus (IExp (IExpVar (Identifier "x")) Equals (IExpInt 5))))) (Type (Identifier "string")))) typeEnv)
                `shouldBe` Nothing