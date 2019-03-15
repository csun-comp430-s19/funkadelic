{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec
import ParserSpec
import TypecheckerSpec

main :: IO ()
main = hspec spec

spec = do
    pSpec 
    tcSpec