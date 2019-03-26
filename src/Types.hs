{-# LANGUAGE FlexibleContexts #-}
module Types where


-- Explicitly create type (type in haskell) called Type (type in our Funkadelic)
-- which derives the typeclasses show and eq
-- Takes in an Identifier in its constructor
data Type = 
        Type Identifier 
    |   FunctionType Type Type
    deriving (Show, Eq)

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
    -- |   ExpLet Identifier Exp Type Exp 
    |   ExpInteger Integer
    |   ExpString String
    |   ExpIExp IExp
    |   ExpLambda Exp Type Exp Type
    |   ExpUnaryFOCall Identifier Exp
    |   ExpNullaryFOCall Identifier
    deriving (Show, Eq)

-- shortcut for constructing a type
mkType :: String -> Type 
mkType t = Type $ Identifier t

mkFuncType :: String -> String -> Type
mkFuncType p r = (FunctionType (Type $ Identifier p) (Type $ Identifier r))