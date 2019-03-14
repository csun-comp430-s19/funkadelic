{-# LANGUAGE FlexibleContexts #-}
module Typechecker where

import Prelude hiding (lookup)
import Parser
import Data.Map

-- Type environment
data Gamma = Gamma [(Identifier, Type)]

getType :: Gamma -> Identifier -> Maybe Type
getType (Gamma g) x = let gMap = fromList g in do
    t <- lookup x gMap
    return t

class Typecheck a where
    typecheck :: a -> Maybe Type

instance Typecheck IExp where
    typecheck iExp = Just $ Type $ Identifier "Int"

instance Typecheck Exp where
    typecheck (ExpInteger ei) = Just $ Type $ Identifier "Int"
    typecheck (ExpString es) = Just $ Type $ Identifier "String"
    typecheck (ExpIExp eie) 
        | typecheck eie == Just (Type $ Identifier "Int") = Just $ Type $ Identifier "Int"
        | otherwise = nothing
    typecheck (ExpLambda e1 t1 e2 t2)
        | paramTypeStatus && returnTypeStatus = Just $ Type $ Identifier t2
        | otherwise = nothing
        where 
            paramTypeStatus = typecheck e1 == Just (Type $ Identifier t1)
            returnTypeStatus = typecheck e2 == Just (Type $ Identifier t2)
