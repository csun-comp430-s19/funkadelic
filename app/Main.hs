module Main where

import System.Environment
import Parser
import Typechecker
import Translator


main = do
    args <- getArgs
    parseResult <- parseProgram $ head args
    putStrLn "output"
    

