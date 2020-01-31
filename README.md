# esl2hs

## What does it do?

Converts abstract syntax specifications from Extended Signature Language to Haskell.

ESL is defined in Ralf Lämmel's book `Software Languages: Syntax, Semantics, and Metaprogramming`, and is made to create abstract syntax specifications.

Note that I write symbols in `camelCase` and types in `PascalCase` to separate them clearly.

## Example

Here is a specification for the abstract syntax of boolean expressions

```
symbol true: -> Expr
symbol false: -> Expr
symbol not: Expr -> Expr
symbol and: Expr, Expr -> Expr
symbol or: Expr, Expr -> Expr
symbol ifElse: Expr, Expr, Expr -> Expr
```

`esl2hs` will will generate the following Haskell code

```haskell
data Expr
  = True
  | False
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | IfElse Expr Expr Expr
```
