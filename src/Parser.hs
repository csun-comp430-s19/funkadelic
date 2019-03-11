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
newtype Identifier = Identifier String deriving (Show, Eq, Ord)

-- iBinaryOperator type
-- Create a type for all possible binary operations
-- ⊗∃IBinOp ::= “+” | “-” | “*” | “/” | “^” | “==”
data IBinOp = Plus | Minus | Mult | Div | Exponent | Equals 
    deriving (Show, Eq)

-- Constructor definition type
-- Equivalent to an AST of CDef
-- cDefParser∃ConstructorDefinition ::= name(τ*)
-- Funkadelic functions (therefore, also constructors) have a maximum arity of one
-- There a CDef can either have a single parameter or no parameters
data CDef =
        UnaryConstructor Identifier Type
    |   NullaryConstructor Identifier
    deriving (Show, Eq)

-- Top level definition type
-- tLd∃TopLevelDefinition :: name “= func(” name “:” Type “):”Type”{” exp “}” | “data” name “=” uDtDef
-- Either a function definition or a data definition
data Tld = 
        FuncDefUnary Identifier Identifier Type Exp Type 
    |   FuncDefNullary Identifier Exp Type
    |   DataDef Identifier [CDef]
    deriving (Show, Eq)

-- iExpression type
-- ie∃IExpression ::= ie1 ⊗ ie2 | name | num+
-- Create a type for all possible expressions
data IExp = 
        IExpInt Integer 
    |   IExpVar Identifier 
    |   IExp IExp IBinOp IExp 
    deriving (Show, Eq)

-- Expression type
-- exp∃Expression ::= x | i | s | ie | \(exp){exp}:τ | name(exp)
data Exp = 
        ExpVariable Identifier
    |   ExpLet Identifier Exp Type Exp 
    |   ExpInteger Integer
    |   ExpString String
    |   ExpIExp IExp
    |   ExpLambda Exp Exp Type
    |   ExpUnaryFOCall Identifier Exp
    |   ExpNullaryFOCall Identifier
    deriving (Show, Eq)

-- Parser for a top level definition
tldParser :: Parser Tld
tldParser = 
    try dDef 
    <|> try nullaryFDef
    <|> unaryFDef

-- Parser for a constructor definition
cDefParser :: Parser CDef
cDefParser = 
    try unaryCDef 
    <|> nullaryCDef

-- parser for let expressions
let' :: Parser Exp
let' = do
    _ <- string "let"
    name <- identifier
    value <- expParser
    _ <- char ':'
    t <- Type <$> identifier
    _ <- char '='
    _ <- string "in"
    exp <- expParser
    return $ ExpLet name value t exp

-- Parser for an expression
expParser :: Parser Exp
expParser = 
    try unaryFOCall
    <|> try let'
    <|> try nullaryFOCall
    <|> try lambda
    <|> ExpIExp <$> (try iExpTerm)
    <|> expAtom

-- Basic units of an expression
expAtom :: Parser Exp
expAtom =   
        ExpVariable <$> identifier
    <|> ExpInteger <$> integer
    <|> ExpString  <$> string'

-- Parser for an integer expression
iExpParser :: Parser IExp
iExpParser = 
    try iExpTerm
    <|> iExpAtom

-- Extract the operands and binary operator
-- Lifts the extracted values into the monad IExp
iExpTerm :: Parser IExp
iExpTerm =  do
    left <- iExpAtom
    binop <- iBinOp
    right <- iExpParser
    return $ IExp left binop right

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

-- Basic units of an integer expression
iExpAtom :: Parser IExp
iExpAtom = 
    IExpInt <$> integer
    <|> IExpVar <$> identifier

-- Extract the name and parameter
-- Lifts the extracted values into the monad UnaryConstructor
unaryCDef :: Parser CDef
unaryCDef = do
    name <- identifier
    _ <- char '('
    paramType <- identifier
    _ <- char ')'
    return $ UnaryConstructor name (Type paramType)

-- Extract the name
-- Lifts the extracted values into the monad NullaryConstructor
nullaryCDef :: Parser CDef
nullaryCDef = do
    name <- identifier
    _ <- string "()"
    return $ NullaryConstructor name

-- Extract the name and constructor definitions
-- Lifts the extracted values into the monad UnaryConstructor
dDef :: Parser Tld
dDef = do
    _ <- string "data"
    name <- identifier
    _ <- char '='
    cDefs <- many1 cDefParser
    return $ DataDef name cDefs -- Note: cDefs is a list of constructor defs

-- Extract the name and parameter name & type, return type, and expression
-- Lifts the extracted values into the monad FuncDefUnary
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
    body <- expParser
    _ <- char '}'
    return $ FuncDefUnary name paramName paramType body retType

-- Extract the name, return type, and expression
-- Lifts the extracted values into the monad FuncDefNullary
nullaryFDef :: Parser Tld
nullaryFDef = do
    name <- identifier
    _ <- string "=func():"
    retType <- Type <$> identifier
    _ <- char '{'
    body <- expParser
    _ <- char '}'
    return $ FuncDefNullary name body retType

-- Extract the parameter, body, and return type
-- Lifts the extracted values into the monad ExpLambda
lambda :: Parser Exp
lambda = do
    _ <- string "\\("
    parameter <- ExpVariable <$> identifier
    _ <- string "){"
    body <- expParser
    _ <- string "}:"
    retType <- identifier 
    return $ ExpLambda parameter body (Type retType)

-- Extract the function name and parameter (only one)
-- Lifts the extracted values into the monad ExpUnaryFOCall
unaryFOCall :: Parser Exp
unaryFOCall = do
    fName <- identifier
    _ <- char '('
    parameter <- expAtom
    _ <- char ')'
    return $ ExpUnaryFOCall fName parameter

-- Extract the function name
-- Lifts the extracted values into the monad ExpNullaryFOCall
nullaryFOCall :: Parser Exp
nullaryFOCall = do
    fName <- identifier
    _ <- string "()"
    return $ ExpNullaryFOCall fName


-- Extract the function name
-- Lifts the extracted values into the monad Identifier
-- Note: Identifiers must start with an alphabetical character
identifier :: Parser Identifier
identifier = do
    first <- count 1 letter -- run count 1 letter and bind it to first
    rest <- many alphaNum
    return $ Identifier (first ++ rest)

-- generates a parser for an arbirtrary type
type' :: Parser Type
type' = Type <$> identifier

-- Ensures an integer is composed of digits 0-9
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

-- Removes spaces from a string
removeSpaces :: String -> String
removeSpaces = filter (/=' ')

parseInput input = parse (many unaryFDef) "failed" (removeSpaces input)

parse' parser input = parse parser "failed" input