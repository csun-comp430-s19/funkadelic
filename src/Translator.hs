{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Translator where

import Parser
import Data.List
import Typechecker

class Translate a g where
    translate :: a -> g -> String

class TranslateTcCall a i g where
    translateTcCall :: a -> i -> g -> String

getParams :: [Identifier] -> String
getParams [(Identifier a)] = a
getParams (Identifier h:tail) = h ++ ", " ++ (getParams tail)

concatNLs :: [String] -> String
concatNLs [] = []
concatNLs (xs:xss) = xs ++ "\n" ++ concatNLs xss

instance Translate IExp Gamma where
    translate (IExpInt a) _ = (show a)
    translate (IExpVar (Identifier a)) _ = a
    translate (IExp ie1 iBinOp ie2) gamma = translate ie1 gamma ++ " "++ (show iBinOp) ++ " " ++ translate ie2 gamma

instance Translate Exp Gamma where
    translate (ExpVariable (Identifier id)) _ = id
    translate (ExpInteger a) _ = (show a)
    translate (ExpString a) _ = (show a)
    translate (ExpTuple exps _) gamma = "{" ++ translateTupleElements exps gamma 1 ++ "}"
    translate (ExpLambda e1 _ e2 _) gamma = "function(" ++ (translate e1 gamma) ++ ") {" ++ (translate e2 gamma) ++ "}"
    translate (ExpIExp a) gamma = (translate a gamma)
    translate (ExpUnaryFOCall (Identifier id) e1) gamma = id ++ "(" ++ (translate e1 gamma) ++ ")"
    translate (ExpNullaryFOCall (Identifier id)) _ = id ++ "()"
    translate (ExpPatternMatchCall e1 _ _ pmes) gamma = "match " ++ (translate e1 gamma) ++ " { " ++ (intercalate (" ") (zipWith translate (pmes) (take (length pmes) (repeat gamma)))) ++ "}"

translateTupleElements' :: [Exp] -> Gamma -> Int -> String -> String
translateTupleElements' [] gamma c str = str
translateTupleElements' (x:xs) gamma c str = translateTupleElements' xs gamma (c + 1) (str ++ (translate x gamma))

translateTupleElements :: [Exp] -> Gamma -> Int -> String
translateTupleElements exps gamma c = translateTupleElements' exps gamma 0 ""

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
    translate (NullaryConstructor (Identifier id)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = id ++ ": null"
    translate (UnaryConstructor (Identifier id) (Type (Identifier t))) _  = id ++ ":{x:adt.any}"

instance Translate Tld Gamma where
    translate (DataDef (Identifier id) c) gamma = "let "++ id ++" =  adt.data({ " ++ (concat $ intersperse "," $ map (flip translate gamma) c) ++ "});" 
    translate (Func (FuncDefUnary (Identifier fName) (Identifier pName) _ e1 _)) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = "function " ++ fName ++ "(" ++ pName ++ ") { " ++ (translate e1 (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ " }"
    translate (Func (FuncDefNullary (Identifier fName) e1 _)) gamma = "function " ++ fName ++ "() { " ++ (translate e1 gamma) ++ " }"
    translate (TypeclassDef _ _) _ = ""
    translate (TypeclassImp (Identifier tcName) imps) (Gamma (Env l, TldMap m, TcDef td, TcImp ti)) = (concatNLs (map imp2Fun imps))
        where 
        imp2Fun (SigImp (Identifier funName) (Type (Identifier inTypeStr)) (Type (Identifier _)) (Identifier paramName) (exp)) = ("function " ++ "_" ++ tcName ++ funName ++ inTypeStr ++ "(" ++ paramName ++ ") { " ++ (translate exp (Gamma (Env l, TldMap m, TcDef td, TcImp ti))) ++ " }")