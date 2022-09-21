{-# LANGUAGE FlexibleContexts #-}
module Language.Lanthorn.Parser where

import Text.ParserCombinators.Parsec

import Language.Lanthorn.AST


--
-- Expr        ::= LetExpr | CaseExpr | IfExpr | Primitive | Application.
-- LetExpr     ::= "let" Name "=" Expr {"," Name "=" Expr} "in" Expr.
-- IfExpr      ::= "if" Expr "then" Expr "else" Expr.
-- Primitive   ::= NumLit | FunLit.
-- FunLit      ::= "fun" "(" Name {"," Name} ")" "->" Expr.
-- Reference   ::= Name [Application].
-- Application ::= "(" [Expr {"," Expr}] ")".
-- NumLit      ::= <<0-9+>> .
-- Name        ::= <<letters>> .
--

--
-- High level: Expressions
--

expr = letRecExpr <|> letExpr <|> ifExpr <|> primExpr <|> reference

letRecExpr = do
    keyword "letrec"
    b <- manyTill (binding) (keyword "in")
    e <- expr
    return (LetRec b e)

letExpr = do
    keyword "let"
    b <- manyTill (binding) (keyword "in")
    e <- expr
    return (LetStar b e)

binding = do
    n <- name
    keyword "="
    e <- expr
    return (n, e)

ifExpr = do
    keyword "if"
    c <- expr
    keyword "then"
    t <- expr
    keyword "else"
    f <- expr
    return (If c t f)

--
-- Primitives
---

primExpr = funLit <|> numLit

funLit = do
    keyword "fun"
    keyword "("
    formals <- sepBy (name) (keyword ",")
    keyword ")"
    keyword "->"
    body <- expr
    return (Fun formals body)

reference = do
    n <- name
    application n <|> return (ValueOf n)

application n = do
    keyword "("
    actuals <- sepBy (expr) (keyword ",")
    keyword ")"
    return (Apply n actuals)

--
-- Low level: Concrete things
--

keyword s = do
    try (string s)
    spaces

name :: Parser String
name = do
    c <- letter
    cs <- many (alphaNum <|> char '_')
    spaces
    return (c:cs)

numLit = do
    ds <- many1 digit
    spaces
    return (NumLit (read ds))

--
-- Driver
--

parseExpr text = parse expr "" text
