{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec
import ParserSpec
import TypecheckerSpec
import IntegrationSpec

main :: IO ()
main = hspec spec

spec = do
    pSpec -- Parser tests
    tcSpec -- Typechecker tests
    -- iSpec -- Integration tests