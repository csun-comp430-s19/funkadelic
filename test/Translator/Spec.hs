{-# LANGUAGE FlexibleContexts #-}
import Parser hiding (type')
import Typechecker
import Translator
import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
    describe "integer expressions" $ do
        it "translates integer expression into javascript" $ do
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp []))
            translate (IExpInt 1) typeEnv `shouldBe` "1"
            translate (IExpVar $ Identifier "x") typeEnv `shouldBe` "x"
            translate (IExp (IExpInt 1) Plus (IExpInt 1)) typeEnv `shouldBe` "1 + 1"
            translate (IExp (IExpVar $ Identifier "x") Minus (IExpInt 2)) typeEnv `shouldBe` "x - 2"
            translate (IExp (IExpInt 4) Mult (IExpVar $ Identifier "x")) typeEnv `shouldBe` "4 * x"
            translate (IExp (IExpVar $ Identifier "x") Div (IExpVar $ Identifier "y")) typeEnv `shouldBe` "x / y"
            translate (IExp (IExpInt 1) Equals (IExpInt 1)) typeEnv `shouldBe` "1 == 1"
            translate (IExp (IExpInt 1) Exponent (IExpInt 1)) typeEnv `shouldBe` "1 ** 1"
    
    describe "expressions" $ do
        it "translates expression into javascript" $ do
            let typeEnv = (Gamma (Env [(Identifier "x", mkType "Int")], TldMap [], TcDef [], TcImp []))
            translate (ExpVariable $ Identifier "x") typeEnv `shouldBe` "x"
            translate (ExpVariable $ Identifier "x23232") typeEnv `shouldBe` "x23232"
            translate (ExpInteger 12323232) typeEnv `shouldBe` "12323232"
            translate (ExpString "xyz") typeEnv `shouldBe` "\"xyz\""
            translate (ExpLambda (ExpVariable $ Identifier "x") (Type $ Identifier "String") (ExpVariable $ Identifier "y") (Type $ Identifier "String")) typeEnv `shouldBe` "function(x) {y}"
            translate (ExpUnaryFOCall (Identifier "name") (ExpVariable $ Identifier "x")) typeEnv `shouldBe` "name(x)"
            translate (ExpNullaryFOCall (Identifier "name")) typeEnv `shouldBe` "name()"
            translateTcCall (TypeclassCallInt (ExpAtomInt 5) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne"))) (Type $ Identifier "Int") typeEnv `shouldBe` "_addaddOneInt(5)"
            translateTcCall (TypeclassCallStr (ExpAtomStr "hello") (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne"))) (Type $ Identifier "Int") typeEnv `shouldBe` "_addaddOneInt(hello)"
            let typeEnv = (Gamma (Env [(Identifier "hi", mkType "Terrible")], TldMap [], TcDef [], TcImp []))
            translateTcCall (TypeclassCallVar (ExpAtomVar (Identifier "hi")) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne"))) (Type $ Identifier "Int") typeEnv `shouldBe` "_addaddOneTerrible(hi)"
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp []))
            translateTcCall (TypeclassCallVar (ExpAtomVar (Identifier "hi")) (Typeclass (Identifier "add")) (TypeclassFunc (Identifier "addOne"))) (Type $ Identifier "Int") typeEnv `shouldBe` "FAIL. TYPE NOT FOUND IN GAMMA FOR hi"
            translate (ExpPatternMatchCall (ExpVariable $ Identifier "x") (Type $ Identifier "funType") (Type $ Identifier "String") [PatternMatchExpression (Identifier "name") [] (ExpString "xyz"), PatternMatchExpression (Identifier "other") [] (ExpString "abc")]) typeEnv `shouldBe` "match x { name: \"xyz\" other: \"abc\"}"

    describe "Constructor Definitionss" $ do
        it "translates Constructor definitions into javascript" $ do
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp []))
            translate (NullaryConstructor $ Identifier "Nullary") typeEnv `shouldBe` "Nullary: null"

    describe "Top Level Definitions" $ do
        it "translates a Top Level Definition into javascript" $ do
            let typeEnv = (Gamma (Env [], TldMap [], TcDef [], TcImp []))
            translate (DataDef (Identifier "x") [NullaryConstructor $ Identifier "Nullary"]) typeEnv `shouldBe` "let x =  adt.data({ Nullary: null});"
            translate (Func (FuncDefUnary (Identifier "funk") (Identifier "a") (Type $ Identifier "string") (ExpVariable $ Identifier "a") (Type $ Identifier "string"))) typeEnv `shouldBe` "function funk(a) { a }"
            translate (Func (FuncDefNullary (Identifier "funk") (ExpVariable $ Identifier "a") (Type $ Identifier "string"))) typeEnv `shouldBe` "function funk() { a }"
            translate (TypeclassImp (Identifier "equals") [SigImp (Identifier "eq") (Type (Identifier "Int")) (Type (Identifier "String")) (Identifier "a") (ExpVariable (Identifier "a")),SigImp (Identifier "eq") (Type (Identifier "Str")) (Type (Identifier "Int")) (Identifier "b") (ExpIExp (IExp (IExpVar (Identifier "b")) Plus (IExpInt 1)))]) typeEnv `shouldBe` "function _equalseqInt(a) { a }\nfunction _equalseqStr(b) { b + 1 }\n"