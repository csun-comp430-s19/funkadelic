{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Translator where

import Parser
import Typechecker
import Data.List

class Translate a g where
    translate :: a -> g -> String

class TranslateTcCall a i g where
    translateTcCall :: a -> i -> g -> String

concatNLs :: [String] -> String
concatNLs [] = []
concatNLs (xs:xss) = xs ++ "\n" ++ concatNLs xss


getParams :: [Identifier] -> String
getParams [(Identifier a)] = a
getParams (Identifier h:tail) = h ++ ", " ++ (getParams tail)

instance Translate IExp Gamma where
    translate (IExpInt a) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = (show a)
    translate (IExpVar (Identifier a)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = a
    translate (IExp ie1 iBinOp ie2) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = translate ie1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) ++ " "++ (show iBinOp) ++ " " ++ translate ie2 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))

instance Translate Exp Gamma where
    translate (ExpVariable (Identifier id)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = id
    translate (ExpInteger a) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = (show a)
    translate (ExpString a) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = (show a)
    translate (ExpLambda e1 _ e2 _) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "function(" ++ (translate e1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ ") {" ++ (translate e2 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ "}"
    translate (ExpIExp a) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = (translate a (Gamma (Env l, TldMap m, TcDef td, TcImp ti)))
    translate (ExpUnaryFOCall (Identifier id) e1) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = id ++ "(" ++ (translate e1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ ")"
    translate (ExpNullaryFOCall (Identifier id)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = id ++ "()"
    translate (ExpPatternMatchCall e1 _ _ pmes) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "match " ++ (translate e1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ " { " ++ (intercalate (" ") (zipWith translate (pmes) (take (length pmes) (repeat (Gamma (Env l, TldMap m, TcDef td, TcImp ti)))))) ++ "}"

instance Translate Pme Gamma where
    translate (PatternMatchExpression (Identifier cid) [] re) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) =  cid ++ ": " ++ (translate re (Gamma (Env l, TldMap m, TcDef td, TcImp ti)))
    translate (PatternMatchExpression (Identifier cid) ((Identifier pid):tail) re) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = cid ++ "(" ++ pid ++ ", " ++ (getParams tail) ++ "): " ++ (translate re (Gamma (Env l, TldMap m, TcDef td, TcImp ti)))
        

instance TranslateTcCall Exp Type Gamma where
    translateTcCall (TypeclassCallInt (ExpAtomInt num) (Typeclass (Identifier tcName)) (TypeclassFunc (Identifier tcFuncName))) (Type (Identifier t)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "_" ++ tcName ++ tcFuncName ++ t ++ "(" ++ show num ++ ")"
    translateTcCall (TypeclassCallStr (ExpAtomStr str) (Typeclass (Identifier tcName)) (TypeclassFunc (Identifier tcFuncName))) (Type (Identifier t)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "_" ++ tcName ++ tcFuncName ++ t ++ "(" ++ str ++ ")"
    translateTcCall (TypeclassCallVar (ExpAtomVar (Identifier var)) (Typeclass (Identifier tcName)) (TypeclassFunc (Identifier tcFuncName))) (Type (Identifier t)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) =
        case getType (Identifier var) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) of
            Nothing -> ("FAIL. TYPE NOT FOUND IN GAMMA FOR " ++ var)
            Just (Type (Identifier t)) -> ( "_" ++ tcName ++ tcFuncName ++ t ++ "(" ++ var ++ ")" )



instance Translate CDef Gamma where
    translate (NullaryConstructor (Identifier id)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = id ++ ":{}"
    -- translate (UnaryConstructor (Identifier id) _) = id ++ "()" -- NEEDS Monadic implementation to generate variable names

instance Translate Tld Gamma where
    translate (DataDef (Identifier id) [c]) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "let " ++ id ++ " = Data(function(){ " ++ (translate c (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ "};" 
    translate (Func (FuncDefUnary (Identifier fName) (Identifier pName) _ e1 _)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "function " ++ fName ++ "(" ++ pName ++ ") { " ++ (translate e1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ " }"
    translate (Func (FuncDefNullary (Identifier fName) e1 _)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "function " ++ fName ++ "() { " ++ (translate e1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ " }"
    translate (TypeclassImp (Identifier tcName) imps) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = (concatNLs (map imp2Fun imps))
        where 
            imp2Fun (SigImp (Identifier funName) (Type (Identifier inTypeStr)) (Type (Identifier _)) (Identifier paramName) (exp)) = ("function " ++ "_" ++ tcName ++ funName ++ inTypeStr ++ "(" ++ paramName ++ ") { " ++ (translate exp (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ " }")
