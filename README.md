# Funkadelic [![Build Status](https://travis-ci.com/csun-comp430-s19/Funkadelic.svg?branch=master)](https://travis-ci.com/csun-comp430-s19/Funkadelic)

## Why this language? 

Funkadelic was designed to be a minimalist statically-typed generic functional programming language.

**Features:**
- Typeclasses (and its prerequisite parametric polymorphism)
- Algebraic data-types, typechecker; Users will be able to define and combine types using sum, product and exponent (function types)
- Pattern matching; Users will be able to pattern match sum types on constructors, and extract data.

## Code snippets

`data myType = Calculator(Integer) | Destroyer(String)`

Here is an example of defining a new type.

`func = funk():string{"a"}`
`func = id(x:string):string{x}`

Here are examples of function declarations.

`funk()`

`id("Hello")`

Here are examples of calling the functions above.

#### Algebraic Data Types

`<1,2,3,4>:<int, int, int, int>`

Here is an example of a product type.

`data sumType = c1(Integer) | c2(String)`

Here is an example of a sum type.

#### Pattern Matching

`"case (X):String{`
    `name()->\"xyz\"`
    `other()->\"abc\""`
`}`

Here is an example of pattern matching. As we can see, our pattern matching is in the form of a case statement which allows for control flow.

#### Typeclasses

`typeclass:add:addOne[A->B]`

Here is a typeclass definition.

`instance:add:addOne[Int->Int]\(b){b+1}`

Here is an implementation of the above typeclass.

`1->add:addOne`

Here is an example of invoking a typeclass from an integer. This also works with any other type.

`typeclass:add:addOne[A->B]`

`instance:add:addTwo[Int->Int]\(b){b+1}`

Notice that addTwo is not defined. Therefore, this will result in a typechecking error.

## Known limitations
Funkadelic only contains functions of maximum arity 1.

## How do I compile your compiler?
- Install <a href="https://docs.haskellstack.org/en/stable/README/">Stack</a>

Docker version (must be in docker directory):
- Install <a href="https://docs.docker.com/install/">Docker</a>
- ./install.sh

## How do I run your compiler?
- stack run Funkadelic-exe IN_FILEPATH

Docker version (must be in docker directory):
- ./compile.sh IN_FILEPATH

## How do I run the compiled code?
Assuming node, npm, and (node install adt) are all installed:
- node out.js

Docker version (must be in docker directory):
- ./execute_compiled.sh

## How do I uninstall your docker image?
(must be in docker directory):
- ./uninstall

## Formal syntax definition
**Syntax:**

- i∃Z x∃Variable s∃String

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

- exp∃TypeclassCall ::= τ "->" typeclass ":" typeclassFunction
 
- cDef∃ConstructorDefinition ::= name(τ∗)

- tLd∃TypeclassDefinition ::= "typeclass:" typeclassName ":" typeclassFunctionName[Generic "->" Generic]

- tLd∃TypeclassImplementation ::= "instance:" typeclassName ":" typeclassFunctionName[τ "->" τ]\(exp)

- uDtDef∃UserDatatypeDefinition ::= cDef | cDef "|" uDtDef
