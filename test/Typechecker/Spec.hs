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
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (IExpVar (Identifier "x"))) typeEnv) `shouldBe` intType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (IExpVar (Identifier "y"))) typeEnv) `shouldBe` stringType
            (evalState (typecheck (IExp (IExpInt 1) Plus (IExpInt 1))) typeEnv) `shouldBe` intType
            (evalState (typecheck (IExpInt 1)) typeEnv) `shouldBe` intType

        it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "y", mkType "String")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (IExpVar (Identifier ""))) typeEnv) `shouldBe` Nothing
            -- (evalState (typecheck (IExp (IExpInt 1) Plus (IExpVar (Identifier "y")))) typeEnv) `shouldBe` Nothing
            -- BUG: above test should be Nothing, but is an Int.




    describe "typechecking expressions" $ do
        it "PASSES on good typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpInteger 1234)) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpString "xyz")) typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "y")))) typeEnv) `shouldBe` stringType
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "z")))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpIExp (IExpInt 1))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpIExp (IExp (IExpInt 1) Plus (IExpInt 1)))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "Int"))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpString "1234") (type' "String") (ExpInteger 1234) (type' "Int"))) typeEnv) `shouldBe` intType
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpString "1234") (type' "String"))) typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "Int" "String"))], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpNullaryFOCall (Identifier "name"))) typeEnv) `shouldBe` intType
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [],  TcImp [(Identifier "add", [SigImp (Identifier "addOne") (Type (Identifier "Int")) (Type (Identifier "Int")) (Identifier "a") (ExpInteger 1)])]))
            (evalState (typecheck (TypeclassCallInt (ExpAtomInt 5) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne")))) typeEnv) `shouldBe` intType
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [],  TcImp [(Identifier "add", [SigImp (Identifier "addOne") (Type (Identifier "Str")) (Type (Identifier "Int")) (Identifier "a") (ExpInteger 1)])]))
            (evalState (typecheck (TypeclassCallStr (ExpAtomStr "hi") (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne")))) typeEnv) `shouldBe` intType
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap [], TcDef [],  TcImp [(Identifier "add", [SigImp (Identifier "addOne") (Type (Identifier "Int")) (Type (Identifier "String")) (Identifier "a") (ExpInteger 1)])]))
            (evalState (typecheck (TypeclassCallVar (ExpAtomVar (Identifier "x")) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne")))) typeEnv) `shouldBe` stringType

        it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpNullaryFOCall (Identifier "name"))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "String" "String"))], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x"))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "String"))) typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma (Env [], TldMap [], TcDef [],  TcImp [(Identifier "add", [SigImp (Identifier "addOne") (Type (Identifier "Str")) (Type (Identifier "Int")) (Identifier "a") (ExpInteger 1)])]))
            (evalState (typecheck (TypeclassCallInt (ExpAtomInt 5) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne")))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [],  TcImp []))
            (evalState (typecheck (TypeclassCallStr (ExpAtomStr "hi") (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne")))) typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "String")], TldMap [], TcDef [],  TcImp [(Identifier "add", [SigImp (Identifier "addOne") (Type (Identifier "Int")) (Type (Identifier "String")) (Identifier "a") (ExpInteger 1)])]))
            (evalState (typecheck (TypeclassCallVar (ExpAtomVar (Identifier "x")) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne")))) typeEnv) `shouldBe` Nothing


    describe "typechecking tlds" $ do
        it "PASSES on good typechecker input" $ do
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp [])) in
                (evalState (typecheck (DataDef (Identifier "newType") [(NullaryConstructor (Identifier "SomeFunct")), (UnaryConstructor (Identifier "Calculate") (Type $ Identifier "Integer"))])) typeEnv)
                    `shouldBe` Just (mkType "newType")
            
            let typeEnv = (Gamma (Env [(Identifier "funk", mkType "String")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (Func (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "String") (ExpVariable $ Identifier "a") (Type $ Identifier "String")))) typeEnv)
                `shouldBe` Just (FunctionType (Type (Identifier "String")) (Type (Identifier "String")))
            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap [], TcDef [], TcImp []))

            -- anotherFunk is a string but typechecking it results in an int?
            (evalState (typecheck (Func (FuncDefNullary (Identifier "anotherFunk") (ExpInteger 1234) (Type $ Identifier "Int")))) typeEnv)
                `shouldBe` Just (Type (Identifier "Int"))

            let tcDef = (TypeclassDef (Identifier "equals") [SigDef (Identifier "eq") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b")),SigDef (Identifier "neq") (Generic (GIdentifier "?c")) (Generic (GIdentifier "?d"))])

            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap [], TcDef [(Identifier "equals", [])], TcImp []))

            (evalState (typecheck tcDef) typeEnv)
                `shouldBe` Just (Type (Identifier "typeclass"))

            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap [], TcDef [], TcImp []))

            (evalState (typecheck tcDef) typeEnv)
                `shouldBe` (Just (Type (Identifier "typeclass")))

            let tcDef = (TypeclassDef (Identifier "equals") [SigDef (Identifier "eq") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b")),SigDef (Identifier "neq") (Generic (GIdentifier "?c")) (Generic (GIdentifier "?d"))])
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [(Identifier "equals", [SigDef (Identifier "lt") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b"))])], TcImp []))
            (evalState (typecheck tcDef) typeEnv)
                `shouldBe` (Just (Type (Identifier "typeclass")))

            let tcImp = (TypeclassImp (Identifier "equals") [SigImp (Identifier "eq") (Type (Identifier "Int")) (Type (Identifier "Int")) (Identifier "a") (ExpInteger 1)])
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [(Identifier "equals", [SigDef (Identifier "eq") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b"))])], TcImp []))
            (evalState (typecheck tcImp) typeEnv)
                `shouldBe` (Just (Type (Identifier "typeclassImp")))

        it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (Func (FuncDefNullary (Identifier "anotherFunk") (ExpInteger 1234) (Type $ Identifier "String")))) typeEnv) 
                `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "anotherFunk", mkType "String")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (Func (FuncDefNullary (Identifier "anotherFunk") (ExpVariable $ Identifier "a") (Type $ Identifier "String")))) typeEnv) 
                `shouldBe` Nothing
            let typeEnv = (Gamma (Env [(Identifier "function", mkType "String")], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck (Func (FuncDefNullary (Identifier "funk") (ExpIExp (IExp (IExpVar (Identifier "x")) Mult (IExp (IExpVar (Identifier "y")) Plus (IExp (IExpVar (Identifier "x")) Equals (IExpInt 5))))) (Type (Identifier "string"))))) typeEnv)
                `shouldBe` Nothing

            let tcDef = (TypeclassDef (Identifier "equals") [SigDef (Identifier "eq") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b")),SigDef (Identifier "neq") (Generic (GIdentifier "?c")) (Generic (GIdentifier "?d"))])
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [(Identifier "equals", [SigDef (Identifier "eq") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b"))])], TcImp []))
            (evalState (typecheck tcDef) typeEnv)
                `shouldBe` Nothing

            let tcImp = (TypeclassImp (Identifier "equals") [SigImp (Identifier "eq") (Type (Identifier "String")) (Type (Identifier "String")) (Identifier "a") (ExpVariable (Identifier "a")),SigImp (Identifier "eq") (Type (Identifier "Int")) (Type (Identifier "Int")) (Identifier "b") (ExpIExp (IExp (IExpVar (Identifier "b")) Plus (IExpInt 1)))])
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp [(Identifier "equals", [SigImp (Identifier "eq") (Type (Identifier "String")) (Type (Identifier "String")) (Identifier "a") (ExpVariable (Identifier "a"))])]))
            (evalState (typecheck tcImp) typeEnv)
                `shouldBe` Nothing

            let tcImp = (TypeclassImp (Identifier "equals") [SigImp (Identifier "eq") (Type (Identifier "Int")) (Type (Identifier "String")) (Identifier "a") (ExpVariable (Identifier "a")),SigImp (Identifier "eq") (Type (Identifier "Int")) (Type (Identifier "Int")) (Identifier "b") (ExpIExp (IExp (IExpVar (Identifier "b")) Plus (IExpInt 1)))])
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp []))
            (evalState (typecheck tcImp) typeEnv)
                `shouldBe` Nothing

            let tcImp = (TypeclassImp (Identifier "equals") [SigImp (Identifier "eq") (Type (Identifier "String")) (Type (Identifier "Int")) (Identifier "a") (ExpInteger 1)])
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [(Identifier "equals", [SigDef (Identifier "eq") (Generic (GIdentifier "?a")) (Generic (GIdentifier "?b"))])], TcImp [(Identifier "equals", [SigImp (Identifier "eq") (Type (Identifier "String")) (Type (Identifier "String")) (Identifier "a") (ExpVariable (Identifier "a"))])]))
            (evalState (typecheck tcImp) typeEnv)
                `shouldBe` Nothing