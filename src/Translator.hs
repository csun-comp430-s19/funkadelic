{-# LANGUAGE FlexibleContexts #-}
module Translator where

import Parser

class Translate a where
    translate :: a -> String

instance Translate IExp where
    translate (IExpInt a) = (show a)
    translate (IExpVar (Identifier a)) = a
    translate (IExp ie1 iBinOp ie2) = translate ie1 ++ " "++ (show iBinOp) ++ " " ++ translate ie2

instance Translate Exp where
    translate (ExpVariable (Identifier id)) = id
    translate (ExpInteger a) = (show a)
    translate (ExpString a) = (show a)
    translate (ExpLambda e1 _ e2 _) = "function(" ++ (translate e1) ++ ") {" ++ (translate e2) ++ "}"
    translate (ExpIExp a) = (translate a)
    translate (ExpUnaryFOCall (Identifier id) e1) = id ++ "(" ++ (translate e1) ++ ")"
    translate (ExpNullaryFOCall (Identifier id)) = id ++ "()"