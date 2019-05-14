{-# LANGUAGE FlexibleContexts #-}
module Typechecker where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Prelude hiding (lookup)
import Parser
import Data.Map
import Data.List

-- Type environment
data Gamma = Gamma (Env, TldMap) deriving (Show)
data Env = Env [(Identifier, Type)] deriving (Show)
data TldMap = TldMap [(Type, [CDef])] deriving (Show)

addEntryToEnv :: Identifier -> Type -> Gamma -> Gamma
addEntryToEnv n t (Gamma (Env l, TldMap m)) = Gamma (Env (l ++ [(n,t)]), TldMap m)

getType :: Identifier -> Gamma -> Maybe Type
getType x (Gamma (Env l, _)) = do
    t <- Data.Map.lookup x gMap
    return t
    where
        gMap = fromList l

getIdentifiers :: Type -> Gamma -> Maybe [CDef]
getIdentifiers t (Gamma (_, TldMap m)) = do
    [c] <- Data.Map.lookup t gMap
    return [c]
    where
        gMap = fromList m

getTypesFromPme :: Pme -> CDef -> (Maybe Type)
getTypesFromPme (PatternMatchExpression a [_] _) (UnaryConstructor i _)  = do
    gamma <- get
    case getType i gamma of
        Just (Type (Identifier t1)) -> do
            case getType a gamma of
                Just (Type (Identifier t2)) -> do
                    case t2 == t1 of
                        True -> return (Type (Identifier t2))
                        False -> Nothing
                Nothing -> Nothing
        Nothing -> Nothing
getTypesFromPme (PatternMatchExpression a [_] _) (NullaryConstructor i) = do
    gamma <- get
    case getType i gamma of
        Just (Type (Identifier t1)) -> do
            case getType a gamma of
                Just (Type (Identifier t2)) -> do
                    case t2 == t1 of
                        True -> return (Type (Identifier t2))
                        False -> Nothing
                Nothing -> Nothing
        Nothing -> Nothing

pmeTypeCheck :: Pme -> (Type, Type) -> (Maybe Type)
pmeTypeCheck (PatternMatchExpression i [_] e) (pt, rt) = do
    gamma <- get
    case getType i gamma of
        Just (Type (Identifier t1)) -> do
            case (Type (Identifier t1)) == pt of
                True -> do
                    et <- typecheck e
                    case et == Just rt of
                        True -> return rt
                        False -> Nothing
                False -> Nothing
        Nothing -> Nothing 

class Typecheck a where
    typecheck :: a -> State Gamma (Maybe Type)


instance Typecheck Tld where
    typecheck (FuncDefUnary fName var inType body outType) = do
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
    typecheck (FuncDefNullary fName body t) = do
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
    typecheck (ExpPatternMatchCall e1 paramType returnType pmes) = do 
        e1t <- typecheck e1
        gamma <- get 
        case e1t == (Just paramType) of
            True -> do
                case getIdentifiers paramType gamma of
                    Just [construct] -> do
                        [matchingType] <- zipWith getTypesFromPme pmes [construct]
                        case elemIndex Nothing [matchingType] of
                            Nothing -> do
                                [typesForCheck] <- Prelude.take (length pmes) (repeat (paramType, returnType))
                                [pmeTypeResults] <- zipWith pmeTypeCheck pmes [typesForCheck]
                                case elemIndex Nothing [pmeTypeResults] of
                                    Nothing -> return (Just returnType)
                                    Just a -> return Nothing
                            Just a -> return Nothing                         
                    Nothing -> return Nothing
            False -> return Nothing