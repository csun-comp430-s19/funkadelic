{-# LANGUAGE FlexibleContexts #-}
module Parser where

import System.IO
import Text.Parsec hiding (try)
import Control.Monad
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (try)
import Text.Parsec.Combinator 
import Text.ParserCombinators.Parsec

-- Explicitly create type (type in haskell) called Type (type in our Funkadelic)
-- which derives the typeclasses show and eq
-- Takes in an Identifier in its constructor
newtype Type = Type Identifier deriving (Show, Eq)

-- Create type called Identifier
-- Constructor takes in a string (Token)
-- Derives typeclasses show and eq
newtype Identifier = Identifier String deriving (Show, Eq)

-- iBinaryOperator type
-- Create a type for all possible binary operations
-- Derives typeclasses show and eq
-- ⊗∃IBinOp ::= “+” | “-” | “*” | “/” | “^” | “==”
data IBinOp = Plus | Minus | Mult | Div | Exponent | Equals 
    deriving (Show, Eq)

-- Constructor definition type
-- cDef∃ConstructorDefinition ::= name(τ*)
-- Funkadelic functions (therefore, also constructors) have a maximum arity of one
-- There a CDef can either have a single parameter or no parameters
-- Derives typeclasses show and eq
data CDef =
    UnaryConstructor Identifier Type
    |   NullaryConstructor Identifier
    deriving (Show, Eq)

-- Top level definition type
-- tLd∃TopLevelDefinition :: name “= func(” name “:” Type “):”Type”{” exp “}” | “data” name “=” uDtDef
-- Either a function definition or a data definition
-- Derives typeclasses show and eq
data Tld = 
    FuncDefUnary Identifier Identifier Type Exp Type 
    |   FuncDefNullary Identifier Exp Type
    |   DataDef Identifier [CDef]
    deriving (Show, Eq)

-- iExpression type
-- ie∃IExpression ::= ie1 ⊗ ie2 | name | num+
-- Create a type for all possible expressions
-- Derives typeclasses show and eq
data IExp = 
    IExpInt Integer 
    |   IExpVar Identifier 
    |   IExp IExp IBinOp IExp 
    deriving (Show, Eq)

-- Function calls type
-- exp∃Expression ::= x | i | s | ie | \(exp){exp}:τ | name(exp)
data Exp = 
    ExpVariable Identifier 
    |   ExpInteger Integer
    |   ExpString String
    |   ExpIExp IExp
    |   ExpLambda Exp Exp Type
    |   ExpUnaryFOCall Identifier Exp
    |   ExpNullaryFOCall Identifier
    deriving (Show, Eq)

-- Check if definition is a data definition or a function definition
tld :: Parser Tld
tld = 
    try dDef 
    <|> try nullaryFDef
    <|> unaryFDef

-- Check the parity of the constructor definition
cDef :: Parser CDef
cDef = 
    try unaryCDef 
    <|> nullaryCDef

exp' :: Parser Exp
exp' = 
    try unaryFOCall
    <|> try nullaryFOCall
    <|> try lambda
    <|> ExpIExp <$> (try iExp')
    <|> expAtom

iExp :: Parser IExp
iExp = 
    try iExp' 
    <|> iExpAtom

-- Extract the parameter
-- Return a unary constructor of type identifier with the extracted parameter
unaryCDef :: Parser CDef
unaryCDef = do
    name <- identifier
    _ <- char '('
    paramType <- identifier
    _ <- char ')'
    return $ UnaryConstructor name (Type paramType)

-- Return a base constructor of type identifier
nullaryCDef :: Parser CDef
nullaryCDef = do
    name <- identifier
    return $ NullaryConstructor name

-- Data definition
-- 
dDef :: Parser Tld
dDef = do
    _ <- string "data"
    name <- identifier
    _ <- char '='
    cDefs <- many1 cDef
    return $ DataDef name cDefs

unaryFDef :: Parser Tld
unaryFDef = do
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
    return $ FuncDefUnary name paramName paramType body retType

nullaryFDef :: Parser Tld
nullaryFDef = do
    name <- identifier
    _ <- string "=func():"
    retType <- Type <$> identifier
    _ <- char '{'
    body <- exp'
    _ <- char '}'
    return $ FuncDefNullary name body retType

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

-- Extract an expression
-- Example: length([5])
-- Where length <- identifier
-- [5] <- parameter
unaryFOCall :: Parser Exp
unaryFOCall = do
    fName <- identifier
    _ <- char '('
    parameter <- expAtom
    _ <- char ')'
    return $ ExpUnaryFOCall fName parameter

nullaryFOCall :: Parser Exp
nullaryFOCall = do
    fName <- identifier
    _ <- string "()"
    return $ ExpNullaryFOCall fName

-- Parses a binary operation
-- If a case does not pass, tries the next case
iBinOp :: Parser IBinOp
iBinOp =    
    (char '+' >> return Plus)
    <|> (char '-' >> return Minus)
    <|> (char '*' >> return Mult)
    <|> (char '/' >> return Div)
    <|> (char '^' >> return Exponent)
    <|> (string "==" >> return Equals)

iExpAtom :: Parser IExp
iExpAtom = 
    IExpInt <$> integer
    <|> IExpVar <$> identifier

-- Takes an iExpression and puts it into context of Parser IExp
iExp' :: Parser IExp
iExp' =  do
    left <- iExpAtom
    binop <- iBinOp
    right <- iExpAtom
    return $ IExp left binop right

--
-- numNumeric ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”
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
character = 
    return <$> nonEscape 
    <|> escape

string' :: Parser String
string' = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

-- Ensures the first character is alphabetical (a letter)
-- Ensures the rest of the string is alphanumeric
identifier :: Parser Identifier
identifier = do
    first <- count 1 letter -- run count 1 letter and bind it to first
    rest <- many alphaNum
    return $ Identifier (first ++ rest)

-- Remove spaces from a string and returns result
removeSpaces :: String -> String
removeSpaces = filter (/=' ')

parseInput input = parse (many unaryFDef) "failed" (removeSpaces input)

parse' parser input = parse parser "failed" input