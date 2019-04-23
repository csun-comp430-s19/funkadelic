# Funkadelic [![Build Status](https://travis-ci.com/csun-comp430-s19/Funkadelic.svg?branch=master)](https://travis-ci.com/csun-comp430-s19/Funkadelic)

### View coverage info for each module
- <a href="https://travis-ci.com/csun-comp430-s19/Funkadelic">Our Latest Travis Build Output</a>

### Dependencies
- <a href="https://docs.haskellstack.org/en/stable/README/">Stack</a>
### Running the tests
- stack build
- stack test --coverage

</br>

## Details

**Language Description:** Minimalist statically-typed generic functional programming language.

**Planned Restrictions:** Funkadelic will only have functions of maximum arity 1.

**Non-Trivial Feature #1:**
Typeclasses (and its prerequisite parametric polymorphism)

**Non-Trivial Feature #2:**
Algebraic data-types, typechecker; Users will be able to define and combine types using sum, product and exponent (function types)

**Non-Trivial Feature #3:** Pattern matching; Users will be able to pattern match sum types on constructors, and extract data.

**Work Planned for Custom Milestone:** Finish the typechecker

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

- exp∃Expression ::= x | i | s | ie | case exp:τ of (pme)+ | \(exp){exp}:τ | name(exp) **Function call**

- exp∃PatternMatchExpressioon ::= name((name { "," name})*) -> exp

- cDef∃ConstructorDefinition ::= name(τ∗)

- uDtDef∃UserDatatypeDefinition ::= cDef | cDef "|" uDtDef
