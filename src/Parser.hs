{-# LANGUAGE FlexibleContexts #-}
module Parser where

import System.IO
import Text.Parsec hiding (try)
import Control.Monad
import Data.Char (isLetter, isDigit, isUpper)
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (try)
import Text.Parsec.Combinator 
import Text.ParserCombinators.Parsec


-- Explicitly create type (type in haskell) called Type (type in our Funkadelic)
-- which derives the typeclasses show and eq
-- Takes in an Identifier in its constructor
data Type = 
        Type Identifier
    |   ProductType [Type]
    |   FunctionType Type Type
    deriving (Show, Eq, Ord)

-- Create type called Identifier
-- Constructor takes in a string (Token)
newtype Identifier = Identifier String deriving (Show, Eq, Ord)

newtype GIdentifier = GIdentifier String deriving (Show, Eq, Ord)

newtype Generic = Generic GIdentifier deriving (Show, Eq)

data Typeclass = Typeclass Identifier deriving (Show, Eq, Ord)

data TypeclassFunc = TypeclassFunc Identifier deriving (Show, Eq, Ord)

-- iBinaryOperator type
-- Create a type for all possible binary operations
-- ⊗∃IBinOp ::= “+” | “-” | “*” | “/” | “**” | “==”
data IBinOp = Plus | Minus | Mult | Div | Exponent | Equals 
    deriving (Eq)

instance Show IBinOp where
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show Equals = "=="
    show Exponent = "**"

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
        Func Function
    |   DataDef Identifier [CDef]
    |   TypeclassDef Identifier [SignatureDef]
    |   TypeclassImp Identifier [SignatureImp]
    deriving (Show, Eq)

data SignatureDef =
    SigDef Identifier Generic Generic deriving (Show, Eq)

data SignatureImp =
    SigImp Identifier Type Type Identifier Exp deriving (Show, Eq)

data Function = 
        FuncDefUnary Identifier Identifier Type Exp Type 
    |   FuncDefNullary Identifier Exp Type
    deriving (Show, Eq)

-- iExpression type
-- ie∃IExpression ::= ie1 ⊗ ie2 | name | num+
-- Create a type for all possible expressions
data IExp = 
        IExpInt Integer 
    |   IExpVar Identifier 
    |   IExp IExp IBinOp IExp 
    deriving (Show, Eq)

data ExpAtom =
        ExpAtomVar Identifier
    |   ExpAtomInt Integer
    |   ExpAtomStr String
    deriving (Show, Eq)

-- Expression type
-- exp∃Expression ::= x | i | s | ie | \(exp){exp}:τ | name(exp)
data Exp = 
        ExpVariable Identifier
    -- |   ExpLet Identifier Exp Type Exp 
    |   ExpTuple [Exp] Type
    |   ExpInteger Integer
    |   ExpString String
    |   ExpIExp IExp
    |   ExpLambda Exp Type Exp Type
    |   ExpUnaryFOCall Identifier Exp
    |   ExpNullaryFOCall Identifier
    |   ExpPatternMatchCall Identifier Type [Pme]
    |   TypeclassCallVar ExpAtom Typeclass TypeclassFunc -- varName tcName tcFuncName
    |   TypeclassCallInt ExpAtom Typeclass TypeclassFunc -- varName tcName tcFuncName
    |   TypeclassCallStr ExpAtom Typeclass TypeclassFunc -- varName tcName tcFuncName
    deriving (Show, Eq)

data Pme =
    PatternMatchExpression Identifier [Identifier] Exp
    deriving(Show, Eq)

parseFile p fname
    = do input <- readFile fname
         return (runP p () fname (removeSpaces input))

parseProgram path = parseFile programParser path

-- shortcut for constructing a type
mkType :: String -> Type 
mkType t = Type $ Identifier t

mkTypeFromIdentifier :: Identifier -> Type
mkTypeFromIdentifier (Identifier s) = mkType s

mkFuncType :: String -> String -> Type
mkFuncType p r = (FunctionType (Type $ Identifier p) (Type $ Identifier r))

programParser :: Parser ([Tld], Exp)
programParser = do
    tlds <- many1 tldParser
    exp <- expParser
    return (tlds, exp)

-- Parser for a top level definition
tldParser :: Parser Tld
tldParser = 
    try dDef 
    <|> try tldFunctionParser
    <|> try tDef
    <|> try tImp

tldFunctionParser :: Parser Tld
tldFunctionParser = do
    function <- functionParser
    return $ Func function

functionParser :: Parser Function
functionParser = 
    try nullaryFDef
    <|> unaryFDef

-- Parser for a constructor definition
cDefParser :: Parser CDef
cDefParser = 
    try unaryCDef 
    <|> nullaryCDef

sigDefParser :: Parser SignatureDef
sigDefParser = do
    sigName <- identifier
    _ <- string "["
    signatureInput <- Generic <$> gIdentifier
    _ <- string "->"
    signatureOutput <- Generic <$> gIdentifier
    _ <- char ']'
    return $ SigDef sigName signatureInput signatureOutput

sigImpParser :: Parser SignatureImp
sigImpParser = do
    sigName <- identifier
    _ <- string "["
    signatureInput <- Type <$> identifier
    _ <- string "->"
    signatureOutput <- Type <$> identifier
    _ <- char ']'
    _ <- char '('
    inputName <- identifier
    _ <- char ')'
    _ <- char '{'
    body <- expParser
    _ <- char '}'
    return $ SigImp sigName signatureInput signatureOutput inputName body

-- parser for let expressions
-- let' :: Parser Exp
-- let' = do
--     _ <- string "let"
--     name <- identifier
--     value <- expParser
--     _ <- char ':'
--     t <- Type <$> identifier
--     _ <- char '='
--     _ <- string "in"
--     exp <- expParser
--     return $ ExpLet name value t exp

-- parser for product type expressions
tuple :: Parser Exp
tuple = do
    _ <- char '<'
    exps <- sepBy expParser (char ',')
    _ <- string ">:<"
    types <- sepBy identifier (char ',')
    _ <- char '>'
    return $ ExpTuple exps $ ProductType (map mkTypeFromIdentifier types)

-- Parser for an expression
expParser :: Parser Exp
expParser = 
        try patternMatchCall
    <|> try unaryFOCall
    -- <|> try let'
    <|> try tuple
    <|> try nullaryFOCall
    <|> try lambda
    <|> try tCallVar
    <|> try tCallInt
    <|> try tCallStr
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
    cDefs <- cDefParser `sepBy` (char '|')
    return $ DataDef name cDefs -- Note: cDefs is a list of constructor defs

tDef :: Parser Tld
tDef = do
    _ <- string "typeclass:"
    name <- identifier
    _ <- char ':'
    sigs <- many1 sigDefParser
    return $ TypeclassDef name sigs

tImp :: Parser Tld
tImp = do
    _ <- string "instance:"
    typeclass <- identifier -- existing typeclass name
    _ <- char ':'
    sigs <- many1 sigImpParser
    return $ TypeclassImp typeclass sigs

tCallVar :: Parser Exp
tCallVar = do
    var <- ExpAtomVar <$> identifier
    _ <- string "->"
    tc <- Typeclass <$> identifier -- existing typeclass name
    _ <- char ':'
    tcFunc <- TypeclassFunc <$> identifier
    return $ TypeclassCallVar var tc tcFunc

tCallInt :: Parser Exp
tCallInt = do
    int <- ExpAtomInt <$> integer
    _ <- string "->"
    tc <- Typeclass <$> identifier -- existing typeclass name
    _ <- char ':'
    tcFunc <- TypeclassFunc <$> identifier
    return $ TypeclassCallInt int tc tcFunc

tCallStr :: Parser Exp
tCallStr = do
    str <- ExpAtomStr <$> string'
    _ <- string "->"
    tc <- Typeclass <$> identifier -- existing typeclass name
    _ <- char ':'
    tcFunc <- TypeclassFunc <$> identifier
    return $ TypeclassCallStr str tc tcFunc


-- Extract the name and parameter name & type, return type, and expression
-- Lifts the extracted values into the monad FuncDefUnary
unaryFDef :: Parser Function
unaryFDef = do
    _ <- string "func="
    name <- identifier
    _ <- char '('
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
nullaryFDef :: Parser Function
nullaryFDef = do
    _ <- string "func="
    name <- identifier
    _ <- string "():"
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
    parameter <- expParser
    _ <- string "):"
    parType <- identifier
    _ <- string "{"
    body <- expParser
    _ <- string "}:"
    retType <- identifier 
    return $ ExpLambda parameter (Type parType) body (Type retType)

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


patternMatchCall :: Parser Exp
patternMatchCall = do
    _ <- string "case("
    pattern <- identifier
    _ <- string "):"
    retType <- Type <$> identifier
    _ <- char '{'
    cases <- many1 pmeParser
    _ <- char '}'
    return $ ExpPatternMatchCall pattern retType cases
    
pmeParser :: Parser Pme
pmeParser = do
    cName <- identifier
    _ <- char '('
    parameters <- identifier `sepBy` (char ',')
    _ <- string ")->"
    returnExp <- expParser
    return $ PatternMatchExpression cName parameters returnExp

-- Extract the function name
-- Lifts the extracted values into the monad Identifier
-- Note: Identifiers must start with an alphabetical character
identifier :: Parser Identifier
identifier = do
    first <- letter -- should be a letter
    rest <- many followingChars
    return $ Identifier (first:rest)
  where
    followingChars = satisfy (\a -> isDigit a || isLetter a || a == '.')

-- Extract the function name
-- Lifts the extracted values into the monad Identifier
-- Note: Identifiers must start with an alphabetical character
gIdentifier :: Parser GIdentifier
gIdentifier = do
    first <- firstChar -- should be a letter
    rest <- many followingChars
    return $ GIdentifier (first:rest)
  where
    firstChar = satisfy (\a -> isLetter a && isUpper a)
    followingChars = satisfy (\a -> isDigit a || isLetter a)


-- generates a parser for an arbitrary type
type' :: Parser Type
type' = Type <$> identifier

-- Ensures an integer is composed of digits 0-9
-- numNumeric ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”
integer :: Parser Integer
integer = do
    n <- many1 digit
    return (read n)


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

-- Removes whitespaces from a string
removeSpaces :: String -> String
removeSpaces = filter (not . flip elem " \r\n\t")

parseInput input = parse (many unaryFDef) "failed" (removeSpaces input)

parse' parser input = parse (spaces *> parser <* eof) "failed" input
