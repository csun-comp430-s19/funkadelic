{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec
import ParserSpec
import TypecheckerSpec
import TranslatorSpec
import IntegrationSpec

main :: IO ()
main = hspec spec

spec = do
    pSpec -- Parser tests
    tcSpec -- Typechecker tests
    trSpec -- Translator tests
    -- iSpec -- Integration tests