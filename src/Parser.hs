{-# LANGUAGE FlexibleContexts #-}
module Parser where

import System.IO
import Text.Parsec hiding (try)
import Control.Monad
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (try)
import Text.Parsec.Combinator 
import Text.ParserCombinators.Parsec

-- Create type called Identifier
-- Constructor takes in a string (Token)
-- Derives typeclasses show and eq
newtype Identifier = Identifier String deriving (Show, Eq)

-- Explicitly create type (type in haskell) called Type (type in our Funkadelic)
-- which derives the typeclasses show and eq
-- Takes in an Identifier in its constructor
newtype Type = Type Identifier deriving (Show, Eq)

-- Create a type for all possible binary operations
-- Derives typeclasses show and eq
-- ⊗[SIGMA]IBinOp ::= “+” | “-” | “*” | “/” | “^” | “==”
data IBinOp = Plus | Minus | Mult | Div | Exponent | Equals deriving (Show, Eq)

-- Constructor definition
-- Funkadelic functions (therefore, also constructors) have a maximum arity of one
-- There a CDef can either have a single parameter or no parameters
-- Derives typeclasses show and eq
data CDef =
        UnaryConstructor Identifier Type
    |   NullaryConstructor Identifier
    deriving (Show, Eq)

data Tld = 
        FuncDef Identifier Identifier Type Exp Type 
    |   DataDef Identifier [CDef]
    deriving (Show, Eq)

-- Create a type for all possible expressions
-- Derives typeclasses show and eq
-- ie[SIGMA]IExpression ::= ie1 ⊗ ie2 | name | num+
data IExp = 
        IExp IExp IBinOp IExp 
    |   IExpVar Identifier 
    |   IExpInt Integer 
    deriving (Show, Eq)

-- exp[SIGMA]Expression ::= x | i | s | ie | \(exp){exp}:τ | name(exp)
data Exp = 
        ExpVariable Identifier 
    |   ExpInteger Integer
    |   ExpString String
    |   ExpIExp IExp
    |   ExpLambda Exp Exp Type
    |   ExpFOCall Identifier Exp
    deriving (Show, Eq)

tld = try dDef <|> fDef

-- Check the parity of the constructor definition
cDef = try unaryCDef <|> nullaryCdef

unaryCDef = do
    name <- identifier
    _ <- char '('
    paramType <- identifier
    _ <- char ')'
    return $ UnaryConstructor name (Type paramType)

-- 
nullaryCdef = do
    name <- identifier
    return $ NullaryConstructor name

dDef = do
    _ <- string "data"
    name <- identifier
    _ <- char '='
    cDefs <- many1 cDef
    return $ DataDef name cDefs

fDef :: Parser Tld
fDef = do
    name <- identifier
    _ <- string "=func("
    paramName <- identifier
    _ <- char ':'
    paramType <- Type <$> identifier
    _ <- string "):"
    retType <- Type <$> identifier
    _ <- char '{'
    body <- exp'
    _ <- char '}'
    return $ FuncDef name paramName paramType body retType

expAtom :: Parser Exp
expAtom =   
        ExpVariable <$> identifier
    <|> ExpInteger <$> integer
    <|> ExpString  <$> string'

lambda :: Parser Exp
lambda = do
    _ <- string "\\("
    parameter <- ExpVariable <$> identifier
    _ <- string "){"
    body <- exp'
    _ <- string "}:"
    retType <- identifier 
    return $ ExpLambda parameter body (Type retType)

fOCall :: Parser Exp
fOCall = do
    fName <- identifier
    _ <- char '('
    parameter <- expAtom
    _ <- char ')'
    return $ ExpFOCall fName parameter


exp' = 
        try fOCall
    <|> try lambda
    <|> ExpIExp <$> (try iExp')
    <|> expAtom

iBinOp :: Parser IBinOp
iBinOp =    
        (char '+' >> return Plus)
    <|> (char '-' >> return Minus)
    <|> (char '*' >> return Mult)
    <|> (char '/' >> return Div)
    <|> (char '^' >> return Exponent)
    <|> (string "==" >> return Equals)

iExpAtom :: Parser IExp
iExpAtom = (IExpInt <$> integer) <|> (IExpVar <$> identifier)

iExp' :: Parser IExp
iExp' =  do
    left <- iExpAtom
    binop <- iBinOp
    right <- iExpAtom
    return $ IExp left binop right

iExp :: Parser IExp
iExp = try iExp' <|> iExpAtom

integer :: Parser Integer
integer = read <$> many1 digit

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf"
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = return <$> nonEscape <|> escape

string' :: Parser String
string' = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

identifier :: Parser Identifier
identifier = do
    first <- count 1 letter
    rest <- many alphaNum
    return $ Identifier (first ++ rest)

removeSpaces :: String -> String
removeSpaces = filter (/=' ')

parseInput input = parse (many fDef) "failed" (removeSpaces input)

parse' parser input = parse parser "failed" input