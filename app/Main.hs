{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment
import Control.Monad.IO.Class
import Parser
import Typechecker
import Translator
import Control.Monad.State.Lazy



main = do
    args <- getArgs
    parseResult <- parseProgram $ head args
    case parseResult of
        Right (tlds, exp) -> do
            gamma <- return $ snd (runState (typecheckProgram (tlds, exp)) emptyGamma)
            putStrLn $ show $ translate exp gamma
            where emptyGamma = (Gamma (Env [], TldMap [], TcDef [], TcImp []))
        Left e -> putStrLn $ show e
    

