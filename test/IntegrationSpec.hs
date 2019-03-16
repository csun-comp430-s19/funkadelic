{-# LANGUAGE FlexibleContexts #-}
module IntegrationSpec where
import Parser hiding (type')
import Typechecker
import Test.Hspec
import Control.Monad.State.Lazy

type' :: String -> Type
type' s = Type $ Identifier s

-- integrationSpec :: IO ()
-- integrationSpec = hspec iSpec

getRight :: Either a b -> Maybe b
getRight y = do 
    Right x <- return y
    return x

parseIExp input = parse' iExpParser input
parseExp input = parse' expParser input
parseTld input = parse' tldParser input

intType = (Just $ type' "Int")
stringType = (Just $ type' "String")


-- iSpec = do
--      describe "integration integer expressions" $ do
--          it "tests integration of typecheck and parser for integer expressions" $ do
