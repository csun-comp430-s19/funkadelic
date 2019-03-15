{-# LANGUAGE FlexibleContexts #-}
module Typechecker where

import Control.Monad.State.Lazy
import Prelude hiding (lookup)
import Parser
import Data.Map

-- Type environment
data Gamma = Gamma [(Identifier, Type)] deriving (Show)



getType :: Gamma -> Identifier -> Maybe Type
getType (Gamma g) x = let gMap = fromList g in do
    t <- lookup x gMap
    return t

class Typecheck a where
    typecheck :: a -> State Gamma String

instance Typecheck IExp where
    typecheck (IExpVar id) = do
        gamma <- get
        case getType gamma id of
            Just (Type (Identifier t)) -> return t
            Nothing -> return "undeclared variable"
    typecheck iExp = return "Int"

main = runState (typecheck x) gamma
            where 
                x = IExpVar (Identifier "var")
                gamma = Gamma [(Identifier "var", Type $ Identifier "string")]

-- instance Typecheck Exp where
--     typecheck (ExpInteger ei) = Just $ Type $ Identifier "Int"
--     typecheck (ExpString es) = Just $ Type $ Identifier "String"
--     typecheck (ExpIExp eie) 
--         | typecheck eie == Just (Type $ Identifier "Int") = Just $ Type $ Identifier "Int"
--         | otherwise = Nothing
--     typecheck (ExpLambda e1 t1 e2 t2)
--         | paramTypeStatus && returnTypeStatus = Just t2
--         | otherwise = Nothing
--         where 
--             paramTypeStatus = typecheck e1 == Just t1
--             returnTypeStatus = typecheck e2 == Just t2
