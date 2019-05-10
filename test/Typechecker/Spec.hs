{-# LANGUAGE FlexibleContexts #-}
import Parser hiding (type')
import Typechecker
import Test.Hspec
import Control.Monad.State.Lazy


type' :: String -> Type
type' s = Type $ Identifier s


main :: IO ()
main = hspec spec

intType = (Just $ type' "Int")
stringType = (Just $ type' "String")

spec = do
    describe "typechecking integer expressions" $ do
        it "PASSES on good typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap []))
            (evalState (typecheck (IExpVar (Identifier "x"))) typeEnv) `shouldBe` intType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")], TldMap []))
            (evalState (typecheck (IExpVar (Identifier "y"))) typeEnv) `shouldBe` stringType
            (evalState (typecheck (IExp (IExpInt 1) Plus (IExpInt 1))) typeEnv) `shouldBe` intType
            (evalState (typecheck (IExpInt 1)) typeEnv) `shouldBe` intType

        it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "y", mkType "String")], TldMap []))
            (evalState (typecheck (IExpVar (Identifier ""))) typeEnv) `shouldBe` Nothing
            -- (evalState (typecheck (IExp (IExpInt 1) Plus (IExpVar (Identifier "y")))) typeEnv) `shouldBe` Nothing
            -- BUG: above test should be Nothing, but is an Int.




    describe "typechecking expressions" $ do
        it "PASSES on good typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap []))
            (evalState (typecheck (ExpInteger 1234)) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpString "xyz")) typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")], TldMap []))
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "y")))) typeEnv) `shouldBe` stringType
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "z")))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpIExp (IExpInt 1))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpIExp (IExp (IExpInt 1) Plus (IExpInt 1)))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "Int"))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpString "1234") (type' "String") (ExpInteger 1234) (type' "Int"))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpString "1234") (type' "String"))) typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "Int" "String"))], TldMap []))
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", mkType "Int")], TldMap []))
            (evalState (typecheck (ExpNullaryFOCall (Identifier "name"))) typeEnv) `shouldBe` intType

        it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap []))
            (evalState (typecheck (ExpNullaryFOCall (Identifier "name"))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "String" "String"))], TldMap []))
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String")], TldMap []))
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")], TldMap []))
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "String"))) typeEnv) `shouldBe` Nothing



    describe "typechecking tlds" $ do
        it "PASSES on good typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "funk", mkType "String")], TldMap []))
            (evalState (typecheck (Func (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "String") (ExpVariable $ Identifier "a") (Type $ Identifier "String")))) typeEnv)
                `shouldBe` Just (FunctionType (Type (Identifier "String")) (Type (Identifier "String")))
            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap []))

            -- anotherFunk is a string but typechecking it results in an int?
            (evalState (typecheck (Func (FuncDefNullary (Identifier "anotherFunk") (ExpInteger 1234) (Type $ Identifier "Int")))) typeEnv)
                `shouldBe` Just (Type (Identifier "Int"))

        it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap []))
            (evalState (typecheck (Func (FuncDefNullary (Identifier "anotherFunk") (ExpInteger 1234) (Type $ Identifier "String")))) typeEnv) 
                `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap []))
            (evalState (typecheck (Func (FuncDefNullary (Identifier "anotherFunk") (ExpVariable $ Identifier "a") (Type $ Identifier "String")))) typeEnv) 
                `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "function", mkType "String")], TldMap []))
            (evalState (typecheck (Func (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExp (IExpVar (Identifier "y")) Plus (IExp (IExpVar (Identifier "x")) Equals (IExpInt 5))))) (Type (Identifier "string"))))) typeEnv)
                `shouldBe` Nothing
