{-# LANGUAGE FlexibleContexts #-}
module Typechecker where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Prelude hiding (lookup)
import Parser
import Data.Map

-- Type environment
data Gamma = Gamma (Env, TldMap, TcDef, TcImp) deriving (Show)
data Env = Env [(Identifier, Type)] deriving (Show)
data TldMap = TldMap [(Type, [CDef])] deriving (Show)
data TcDef = TcDef [(Identifier, [SignatureDef])] deriving (Show)
data TcImp = TcImp [(Identifier, [SignatureImp])] deriving (Show)


addEntryToEnv :: Identifier -> Type -> Gamma -> Gamma
addEntryToEnv n t (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = Gamma (Env (l ++ [(n,t)]), TldMap m, TcDef td, TcImp ti)

getType :: Identifier -> Gamma -> Maybe Type
getType x (Gamma (Env l, _, _, _)) = do
    t <- lookup x gMap
    return t
    where
        gMap = fromList l

getIdentifiers :: Type -> Gamma -> Maybe [CDef]
getIdentifiers t (Gamma (_, TldMap m, _, _)) = do
    [c] <- lookup t gMap
    return [c]
    where
        gMap = fromList m

getTcDefIdentifiers :: Identifier -> Gamma -> Maybe [SignatureDef]
getTcDefIdentifiers tc (Gamma (_, _, TcDef td, _)) = do
    [sigDefs] <- lookup tc gMap
    return [sigDefs]
    where
        gMap = fromList td 

getTcImpIdentifiers :: Identifier -> Gamma -> Maybe [SignatureImp]
getTcImpIdentifiers tc (Gamma (_, _, _, TcImp ti)) = do
    [sigImps] <- lookup tc gMap
    return [sigImps]
    where
        gMap = fromList ti


-- checkPme :: Type -> Type -> Identifier -> [Identifier] -> Exp -> Maybe Type
-- checkPme pType rType cName _ e1 = do
--     case (Just pType) == typecheck cName of
--         True -> do
--             case (Just rType) == typecheck e1 of
--                 True -> return (Just rType)
--                 False -> return Nothing
--         False -> return Nothing

class Typecheck a where
    typecheck :: a -> State Gamma (Maybe Type)


instance Typecheck Tld where
    typecheck (Func (FuncDefUnary fName var inType body outType)) = do
        gamma <- get
        _ <- put $ addEntryToEnv var inType gamma
        actualOutType <- typecheck body
        case actualOutType == Just outType of
            True -> do
                _ <- put $ addEntryToEnv fName functionType gamma
                return (Just functionType)
                where
                    functionType = FunctionType inType  outType
            False -> return Nothing
    typecheck (Func (FuncDefNullary fName body t)) = do
        actualType <- typecheck body
        case actualType == Just t of
            True -> do
                gamma <- get
                _ <- put $ addEntryToEnv fName t gamma
                return $ Just t
            False -> return Nothing


    


instance Typecheck IExp where
    typecheck (IExpVar id) = do
        gamma <- get
        case getType id gamma of
            Just (Type (Identifier t)) -> return (Just $ mkType $ t)
            Nothing -> return Nothing
    typecheck iExp = return (Just $ mkType $ "Int")

instance Typecheck Exp where
    typecheck (ExpInteger ei) = return $ Just $ mkType "Int"
    typecheck (ExpString es) = return $ Just $ mkType "String"
    typecheck (ExpIExp eie) = typecheck eie
    typecheck (ExpVariable x) = do
        gamma <- get
        return $ getType x gamma
    typecheck (ExpLambda e1 t1 e2 t2) = do
        e1t <- typecheck e1
        e2t <- typecheck e2
        case e1t == (Just t1) && e2t == (Just t2) of
            True -> return (Just t2)
            False -> return Nothing
    typecheck (ExpUnaryFOCall fName param) = do
        actualType <- typecheck param
        gamma <- get
        case getType fName gamma of
            Just (FunctionType inType outType) -> do
                case typesMatch of
                    True -> return (Just outType)
                    False -> return Nothing
                where
                    typesMatch = (Just inType) == actualType
            otherwise -> return Nothing
    typecheck (ExpNullaryFOCall fName) = do
        gamma <- get
        case getType fName gamma of
            Just t -> return (Just t)
            Nothing -> return Nothing
    -- typecheck (ExpPatternMatchCall e1 paramType returnType Pmes) = do 
    --     e1t <- typecheck e1
    --     gamma <- get 
    --     case e1t == (just paramType) of
    --         True -> do
                

    --         False -> return Nothing