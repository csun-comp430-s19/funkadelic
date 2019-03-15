{-# LANGUAGE FlexibleContexts #-}
module Typechecker where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Prelude hiding (lookup)
import Parser
import Data.Map

-- Type environment
data Gamma = Gamma [(Identifier, Type)] deriving (Show)

addEntry :: Identifier -> Type -> Gamma -> Gamma
addEntry n t (Gamma l) = Gamma (l ++ [(n,t)])

getType :: Gamma -> Identifier -> Maybe Type
getType (Gamma g) x = let gMap = fromList g in do
    t <- lookup x gMap
    return t

class Typecheck a where
    typecheck :: a -> State Gamma (Maybe Type)
    

instance Typecheck Tld where
    typecheck (FuncDefUnary fName cName inType body outType) = do
        actualOutType <- typecheck body
        case actualOutType == (Just outType) of
            True -> do
                gamma <- get
                _ <- put $ addEntry fName functionType gamma
                return (Just functionType)
                where
                    functionType = FunctionType inType  outType
            False -> return Nothing


instance Typecheck IExp where
    typecheck (IExpVar id) = do
        gamma <- get
        case getType gamma id of
            Just (Type (Identifier t)) -> return (Just $ Type $ Identifier $ t)
            Nothing -> return Nothing
    typecheck iExp = return (Just $ Type $ Identifier $ "Int")

instance Typecheck Exp where
    typecheck (ExpInteger ei) = return $ Just $ Type $ Identifier "Int"
    typecheck (ExpString es) = return $ Just $ Type $ Identifier "String"
    typecheck (ExpIExp eie) = typecheck eie
    typecheck (ExpLambda e1 t1 e2 t2) = do
        e1t <- typecheck e1
        e2t <- typecheck e2
        case e1t == (Just t1) && e2t == (Just t2) of
            True -> return (Just t1)
            False -> return Nothing

main = runState (typecheck x) gamma
            where 
                x = IExpVar (Identifier "var")
                gamma = Gamma [(Identifier "var", Type $ Identifier "string")]
