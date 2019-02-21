# Funkadelic

[![Build Status](https://travis-ci.com/csun-comp430-s19/Funkadelic.svg?branch=master)](https://travis-ci.com/csun-comp430-s19/Funkadelic)

### Dependencies
- <a href="https://docs.haskellstack.org/en/stable/README/">Stack</a>
### Running the tests
- stack build
- stack test --coverage

</br>

## Details

**Language Description:** Minimalist statically-typed generic functional programming language.

**Planned Restrictions:** Funkadelic will only have functions of maximum arity 1.

**Syntax:**

i∃Z        x∃Variable s∃String

τ∃Type ::= &quot;int&quot; | &quot;string&quot; |τ &quot;-\&gt;&quot; τ **Function type** | uDtName **A user defined type can contain 0 or more other types**

tLd∃TopLevelDefinition :: name &quot;= func(&quot; name &quot;:&quot; Type &quot;):&quot;Type&quot;{&quot; exp &quot;}&quot; | &quot;data&quot; name &quot;=&quot; uDtDef

uDtName∃UserDefinedName ::= name

ie∃IExpression ::= ie1 ⊗ ie2 | name | num+

⊗∃IBinOp ::= &quot;+&quot; | &quot;-&quot; | &quot;\*&quot; | &quot;/&quot; | &quot;^&quot; | &quot;==&quot;

name∃Name ::= alphaUpper |alpha name| namenum | name alpha **Function name must start with alpha character**

alpha∃AlphaCharacter ::= alphaLower | alphaUpper

alphaLower∃AlphaLowerCaseCharacter ::= &quot;a&quot; | &quot;b&quot; | &quot;c&quot; …

alphaUpper∃AlphaUpperCaseCharacter ::= &quot;a&quot; | &quot;b&quot; | &quot;c&quot; …

num∃Numeric ::= &quot;0&quot; | &quot;1&quot; | &quot;2&quot; | &quot;3&quot; | &quot;4&quot; | &quot;5&quot; | &quot;6&quot; | &quot;7&quot; | &quot;8&quot; | &quot;9&quot;

fDef∃FunctionDefinition ::= name(exp1:τ):τ{exp2} | name():τ{exp1}

exp∃Expression ::= x | i | s | ie | \(exp){exp}:τ | name(exp) **Function call**

cDef∃ConstructorDefinition ::= name(τ∗)

uDtDef∃UserDatatypeDefinition ::= cDef | cDef &quot;|&quot; uDtDef

**Non-Trivial Feature #1:**
Typeclasses (and its prerequisite parametric polymorphism)

**Non-Trivial Feature #2:**
Algebraic data-types, typechecker; Users will be able to define and combine types using sum, product and exponent (function types)

**Non-Trivial Feature #3:** Pattern matching; Users will be able to pattern match sum types on constructors, and extract data.

**Work Planned for Custom Milestone:** Finish the typechecker
