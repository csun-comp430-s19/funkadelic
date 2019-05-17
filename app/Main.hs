module Main where

import System.Environment
import Control.Monad.IO.Class
import Parser
import Typechecker
import Translator


main = do
    args <- getArgs
    parseResult <- parseProgram $ head args
    case parseResult of
        Right (tlds, exp) -> do
            gamma <- return $ typecheckProgram (tlds, exp)
            return $ translate exp gamma
        Left _ -> undefined
    

