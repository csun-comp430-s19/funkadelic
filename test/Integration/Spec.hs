{-# LANGUAGE FlexibleContexts #-}
import Parser hiding (type')
import Typechecker
import Types
import Test.Hspec
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

parseIExpAndTypecheck :: String -> Gamma -> Maybe Type
parseIExpAndTypecheck x y = do
    parseResultWrapped <- return (parseIExp x)
    Just parseResult <- return (getRight parseResultWrapped)
    typeResult <- return (evalState (typecheck parseResult) y)
    Just result <- return typeResult
    return result

-- Can this be simplified into one expression?
parseExpAndTypecheck :: String -> Gamma -> Maybe Type
parseExpAndTypecheck x y = do
    parseResultWrapped <- return (parseExp x)
    Just parseResult <- return (getRight parseResultWrapped)
    typeResult <- return (evalState (typecheck parseResult) y)
    Just result <- return typeResult
    return result

parseTldAndTypecheck :: String -> Gamma -> Maybe Type
parseTldAndTypecheck x y = do
    parseResultWrapped <- return (parseTld x)
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
            (parseIExpAndTypecheck "1+1" typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseIExpAndTypecheck "x" typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseIExpAndTypecheck "1" typeEnv) `shouldBe` intType

            (parseIExpAndTypecheck "x*1+2-3/4" typeEnv) `shouldBe` intType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            (parseIExpAndTypecheck "y" typeEnv) `shouldBe` stringType

         it "FAILS on bad parse input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseIExpAndTypecheck "2*_" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "2*8a" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "3^+2" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "3^" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "x23232 + _2" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "_2+(2)" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "" typeEnv) `shouldBe` Nothing
            (parseIExpAndTypecheck "\\(x):String{x}:String" typeEnv) `shouldBe` Nothing

         -- it "FAILS on bad typechecker input" $ do
         --    let typeEnv = (Gamma [(Identifier "x", mkType "String")])
            -- (parseIExpAndTypecheck "2*x" typeEnv) `shouldBe` Nothing
            -- Above is a bug and should be nothing, since it is int * string




     describe "integrate expressions with parser and typechecker" $ do
         it "PASSES on good parse input, good typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseExpAndTypecheck "1234" typeEnv) `shouldBe` intType
            (parseExpAndTypecheck "\"xyz\"" typeEnv) `shouldBe` stringType
            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            (parseExpAndTypecheck "y" typeEnv) `shouldBe` stringType
            (evalState (typecheck (ExpIExp (IExpVar (Identifier "z")))) typeEnv) `shouldBe` intType
            (parseExpAndTypecheck "1" typeEnv) `shouldBe` intType
            (parseExpAndTypecheck "1+1" typeEnv) `shouldBe` intType

            -- (parseExpAndTypecheck "\\(1234):Int{1234}:Int" typeEnv) `shouldBe` intType
            -- (parseExpAndTypecheck "\\(1234):String{1234}:Int" typeEnv) `shouldBe` intType
            -- (parseExpAndTypecheck "\\(1234):Int{1234}:String" typeEnv) `shouldBe` stringType
            --- Tests above depend on if lambda is changed to have only variables as parameters

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "Int" "String"))])
            (parseExpAndTypecheck "name(x)" typeEnv) `shouldBe` stringType

            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", mkType "Int")])
            (parseExpAndTypecheck "name()" typeEnv) `shouldBe` intType

         it "FAILS on bad parse input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseExpAndTypecheck "name(x" typeEnv) `shouldBe` Nothing

         it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseExpAndTypecheck "name()" typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "name", (mkFuncType "String" "String"))])
            (parseExpAndTypecheck "name(x)" typeEnv) `shouldBe` Nothing
            let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String")])
            (parseExpAndTypecheck "name(x)" typeEnv) `shouldBe` Nothing

            --let typeEnv = (Gamma [(Identifier "x", mkType "Int"), (Identifier "y", mkType "String"), (Identifier "z", mkType "Int")])
            --(evalState (typecheck (ExpLambda (ExpInteger 1234) (type' "Int") (ExpInteger 1234) (type' "String"))) typeEnv) `shouldBe` Nothing


     describe "integrate tlds with parser and typechecker" $ do
         it "PASSES on good parse input, good typechecker input" $ do

            let typeEnv = (Gamma [(Identifier "funk", mkType "String")])
            (parseTldAndTypecheck "funk=func(a:String):String{a}" typeEnv) `shouldBe` Just (FunctionType (Type (Identifier "String")) (Type (Identifier "String")))

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "Int")])
            (parseTldAndTypecheck "funk=func():Int{1234}" typeEnv) `shouldBe` Just (Type (Identifier "Int"))


         it "FAILS on bad typechecker input" $ do
            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (parseTldAndTypecheck "anotherFunk=func():String{1234}" typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "anotherFunk", mkType "String")])
            (parseTldAndTypecheck "anotherFunk=func():String{a}" typeEnv) `shouldBe` Nothing

            let typeEnv = (Gamma [(Identifier "function", mkType "String")])
            (parseTldAndTypecheck "anotherFunk=func():String{x+y+x=5}" typeEnv) `shouldBe` Nothing
