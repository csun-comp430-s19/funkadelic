# Funkadelic [![Build Status](https://travis-ci.com/csun-comp430-s19/Funkadelic.svg?branch=master)](https://travis-ci.com/csun-comp430-s19/Funkadelic)

## Why this language? 



Funkadelic was designed to be a minimalist statically-typed generic functional programming language.

**Features:**
- Typeclasses (and its prerequisite parametric polymorphism)
- Algebraic data-types, typechecker; Users will be able to define and combine types using sum, product and exponent (function types)
- Pattern matching; Users will be able to pattern match sum types on constructors, and extract data.

## Code snippets

datanewType=Calculate(Integer)

funk=func():string{a}

<1,2,3,4>

case 12:Integer of:String name()->\"xyz\"

typeclass:add:addOne[A->B]

instance:add:addOne[Int->Int]\(b){b+1}

1->add::addOne
## Known limitations
Funkadelic only contains functions of maximum arity 1.
## Knowing what you know now, what would you do differently?
Learn Haskell more along the way to be able to use the language more effectively. Designed a way to compile a file all the way to its translation.
## How do I compile your compiler?
- <a href="https://docs.haskellstack.org/en/stable/README/">Stack</a>
## How do I run your compiler?
- stack build
- stack test --coverage
## Formal syntax definition
**Syntax:**

- i∃Z        x∃Variable s∃String

- τ∃Type ::= "int" | "string" |τ "->" τ **Function type** | uDtName **A user defined type can contain 0 or more other types**

- tLd∃TopLevelDefinition :: name "= func(" name ":" Type "):"Type"{" exp "}" | "data" name "=" uDtDef

- uDtName∃UserDefinedName ::= name

- ie∃IExpression ::= ie1 ⊗ ie2 | name | num+

- ⊗∃IBinOp ::= "+" | "-" | "\*" | "/" | "^" | "=="

- name∃Name ::= alphaUpper |alpha name| namenum | name alpha **Function name must start with alpha character**

- alpha∃AlphaCharacter ::= alphaLower | alphaUpper

- alphaLower∃AlphaLowerCaseCharacter ::= "a" | "b" | "c" …

- alphaUpper∃AlphaUpperCaseCharacter ::= "A" | "B" | "C" …

- num∃Numeric ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

- fDef∃FunctionDefinition ::= name(exp1:τ):τ{exp2} | name():τ{exp1}

- exp∃Expression ::= x | i | s | ie | case exp:τ of:τ (pme)+ | \(exp){exp}:τ | name(exp) **Function call**

- exp∃PatternMatchExpression ::= name((name { "," name})*)->exp

- exp∃TypeclassCall ::= τ->typeclass::typeclassFunction
 
- cDef∃ConstructorDefinition ::= name(τ∗)

- tLd∃TypeclassDefinition ::= "typeclass:" typeclassName ":" typeclassFunctionName[Generic "->" Generic]


- tLd∃TypeclassImplementation ::= "instance:" typeclassName ":" typeclassFunctionName[τ "->" τ]\(exp)

- uDtDef∃UserDatatypeDefinition ::= cDef | cDef "|" uDtDef