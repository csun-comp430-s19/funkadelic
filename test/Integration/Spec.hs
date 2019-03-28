{-# LANGUAGE FlexibleContexts #-}
import Parser hiding (type')
import Typechecker
import Test.Hspec
import Control.Monad.Fail
import Control.Monad.State.Lazy

main :: IO ()
main = hspec spec

getRight :: Either a b -> Maybe b
getRight y = do 
    Right x <- return y
    return x

parseIExp input = parse' iExpParser input
parseExp input = parse' expParser input
parseTld input = parse' tldParser input

parseAndTypecheck :: (Typecheck a1, Control.Monad.Fail.MonadFail m) =>
     (t -> Either a2 a1) -> t -> Gamma -> m Type
parseAndTypecheck f x y = do
    parseResultWrapped <- return (f x)
    Just parseResult <- return (getRight parseResultWrapped)
    typeResult <- return (evalState (typecheck parseResult) y)
    Just result <- return typeResult
    return result

type' :: String -> Type
type' s = Type $ Identifier s

intType = (Just $ type' "Int")
stringType = (Just $ type' "String")


spec = do
     describe "integrate integer expressions with parser and typechecker" $ do
         it "PASSES on good parse input, good typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseIExp "1+1" typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseIExp "x" typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseIExp "1" typeEnv) `shouldBe` intType

            (parseAndTypecheck parseIExp "x*1+2-3/4" typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            (parseAndTypecheck parseIExp "y" typeEnv) `shouldBe` stringType

         it "FAILS on bad parse input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseIExp "2*_" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "2*8a" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "3^+2" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "3^" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "x23232 + _2" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "_2+(2)" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseIExp "\\(x):String{x}:String" typeEnv) `shouldBe` Nothing

         -- it "FAILS on bad typechecker input" $ do
         --    let typeEnv = (Gamma [(Identifier "x", mkType "String")])
            -- (parseAndTypecheck "2*x" typeEnv) `shouldBe` Nothing
            -- Above is a bug and should be nothing, since it is int * string




     describe "integrate expressions with parser and typechecker" $ do
         it "PASSES on good parse input, good typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseExp "1234" typeEnv) `shouldBe` intType
            (parseAndTypecheck parseExp "\"xyz\"" typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            (parseAndTypecheck parseExp "y" typeEnv) `shouldBe` stringType
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "z")))) typeEnv) `shouldBe` intType
            (parseAndTypecheck parseExp "1" typeEnv) `shouldBe` intType
            (parseAndTypecheck parseExp "1+1" typeEnv) `shouldBe` intType

            (parseAndTypecheck parseExp "\\(1234):Int{1234}:Int" typeEnv) `shouldBe` intType
            (parseAndTypecheck parseExp "\\(1234):Int{\"1234\"}:String" typeEnv) `shouldBe` stringType


            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "Int" "String"))])
            (parseAndTypecheck parseExp "name(x)" typeEnv) `shouldBe` stringType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", mkType "Int")])
            (parseAndTypecheck parseExp "name()" typeEnv) `shouldBe` intType

         it "FAILS on bad parse input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseExp "name(x" typeEnv) `shouldBe` Nothing

         it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck parseExp "name()" typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "String" "String"))])
            (parseAndTypecheck parseExp "name(x)" typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String")])
            (parseAndTypecheck parseExp "name(x)" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseExp "\\(1234):String{1234}:Int" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseExp "\\(1234):String{1234}:String" typeEnv) `shouldBe` Nothing
            (parseAndTypecheck parseExp "\\(1234):Int{1234}:String" typeEnv) `shouldBe` Nothing



     describe "integrate tlds with parser and typechecker" $ do
         it "PASSES on good parse input, good typechecker input" $ do

            let typeEnv = (Gamma [(Identifier "funk", mkType "String")])
            (parseAndTypecheck parseTld "funk=func(a:String):String{a}" typeEnv) `shouldBe` Just (FunctionType (Type (Identifier "String")) (Type (Identifier "String")))

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "Int")])
            (parseAndTypecheck parseTld "funk=func():Int{1234}" typeEnv) `shouldBe` Just (Type (Identifier "Int"))


         it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (parseAndTypecheck parseTld "anotherFunk=func():String{1234}" typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (parseAndTypecheck parseTld "anotherFunk=func():String{a}" typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "function", mkType "String")])
            (parseAndTypecheck parseTld "anotherFunk=func():String{x+y+x=5}" typeEnv) `shouldBe` Nothing
