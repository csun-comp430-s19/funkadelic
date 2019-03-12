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
    typecheck :: Maybe a -> Maybe Type

instance Typecheck IExp where
    typecheck (Just iExp) = Just $ Type $ Identifier "Int"
    typecheck Nothing = Nothing

instance Typecheck Exp where
    typecheck (Just (ExpInteger a)) = Just $ Type $ Identifier "Int"
    typecheck (Just (ExpString a)) = Just $ Type $ Identifier "String"
    typecheck (Just (ExpIExp a)) = typecheck (Just a)
    typecheck Nothing = Nothing

