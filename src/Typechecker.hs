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
    typecheck (ExpInteger a) = Just $ Type $ Identifier "Int"
    typecheck (ExpString a) = Just $ Type $ Identifier "String"
    typecheck (ExpIExp a) = Just $ Type $ Identifier "Int"