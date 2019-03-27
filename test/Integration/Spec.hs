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

parseAndTypecheck :: String -> Gamma -> Maybe Type
parseAndTypecheck x y = do
    parseResultWrapped <- return (parseIExp x)
    Just parseResult <- return (getRight parseResultWrapped)
    typeResult <- return (evalState (typecheck parseResult) y)
    Just result <- return typeResult
    return result

type' :: String -> Type
type' s = Type $ Identifier s

intType = (Just $ type' "Int")
stringType = (Just $ type' "String")


spec = do
     describe "integrates parser and typechecker" $ do
         it "tests integration of typecheck and parser for integer expressions" $ do
            let typeEnv = (Gamma [(Identifier "x", mkType "Int")])
            (parseAndTypecheck "1+1" typeEnv) `shouldBe` intType