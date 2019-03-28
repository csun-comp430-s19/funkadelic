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

instance Translate CDef where
    translate (NullaryConstructor (Identifier id)) = id ++ "()"
    -- translate (UnaryConstructor (Identifier id) _) = id ++ "()" -- NEEDS Monadic implementation to generate variable names

instance Translate Tld where
    translate (DataDef (Identifier id) [c]) = "let " ++ id ++ " = new " ++ (translate c) ++ ";" 
    translate (FuncDefUnary (Identifier fName) (Identifier pName) _ e1 _) = "function " ++ fName ++ "(" ++ pName ++ ") { " ++ (translate e1) ++ " }"
    translate (FuncDefNullary (Identifier fName) e1 _) = "function " ++ fName ++ "() { " ++ (translate e1) ++ " }"
