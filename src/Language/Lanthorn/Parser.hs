{-# LANGUAGE FlexibleContexts #-}
module Language.Lanthorn.Parser where

import Text.ParserCombinators.Parsec

import Language.Lanthorn.AST


-- Body ::= Expr.
-- Expr ::= Import
--        | Export
--        | LetExpr
--        | IfExpr
--        | LetRec
--        | Primitive
--        | RefOrApp
--        .
-- Import ::= "import" {ImportFrom} "in" Expr.
-- ImportFrom ::= ImportSpec {"," ImportSpec} "from" ModuleRef.
-- ImportSpec ::= Name ["as" Name ["infixl" IntLit]].
-- ModuleRef ::= "module" "(" StrLit ")" | "builtins".
--
-- Export ::= "export" "(" Name {"," Name} ")".
--
-- LetExpr     ::= "let" Name "=" Expr {"," Name "=" Expr} "in" Expr.
-- IfExpr      ::= "if" Expr "then" Expr "else" Expr.
-- LetRec ::= "letrec" {Binding} "in" Expr.
-- Binding ::= Name "=" Expr.
--
-- RefOrApp ::= Name ["(" Expr {"," Expr} ")"].
--
-- Primitive = IntLit | StrLit | FunLit
-- IntLit ::= <the usual>
-- StrLit ::= <the usual>
-- FunLit ::= "fun" "(" [Name {"," Name}] ")" "->" Expr.

data ParseContext = ParseContext Integer

--
-- High level: Expressions
--

expr p = (importExpr p) <|> (exportExpr p) <|> (letRecExpr p) <|> (letExpr p) <|> (ifExpr p) <|> (primitive p) <|> (refOrApp p)

--
-- import
--

importExpr pctx = do
    keyword "import"
    i <- many (importFrom)
    keyword "in"
    -- TODO create modified parse context here
    e <- expr pctx
    return $ Import i e

importFrom = do
    notFollowedBy (keyword "in")
    i <- sepBy (importSpec) (keyword ",")
    keyword "from"
    e <- moduleSpec  -- TODO in a better world this would be an expression
    return $ ImportFrom i e

importSpec = do
    n <- name
    nn <- option Nothing importAs
    return $ ImportSpec n nn

importAs = do
    keyword "as"
    nn <- name
    return $ Just nn

moduleSpec = builtinsModule <|> namedModule

builtinsModule = do
    keyword "builtins"
    return $ Builtins

namedModule = do
    keyword "module"
    keyword "("
    (StrLit s) <- strLit
    keyword ")"
    return $ ModuleSpec s

--
-- export
--

exportExpr pctx = do
    keyword "export"
    keyword "("
    es <- sepBy (name) (keyword ",")
    keyword ")"
    return $ Export es

--
-- letrec
--

letRecExpr pctx = do
    keyword "letrec"
    b <- many (binding pctx)
    keyword "in"
    e <- expr pctx
    return $ LetRec b e

binding pctx = do
    notFollowedBy (keyword "in")
    n <- name
    keyword "="
    e <- expr pctx
    return (n, e)

--
-- others
--

letExpr pctx = do
    keyword "let"
    b <- many (binding pctx)
    keyword "in"
    e <- expr pctx
    return $ LetStar b e

ifExpr pctx = do
    keyword "if"
    c <- expr pctx
    keyword "then"
    t <- expr pctx
    keyword "else"
    f <- expr pctx
    return $ If c t f

--
-- Primitives
---

primitive pctx = (funLit pctx) <|> intLit <|> strLit

funLit pctx = do
    keyword "fun"
    keyword "("
    formals <- sepBy (name) (keyword ",")
    keyword ")"
    keyword "->"
    body <- expr pctx
    return (Fun formals body)

strLit = do
    string "'"
    sentinel <- many $ satisfy (\x -> x /= '\'')
    string "'"
    contents <- many $ satisfy (\x -> x /= '\'')
    string "'"
    (try $ stringTail sentinel contents) <|> (stringCont sentinel contents)

stringCont sentinel contents = do
    contents' <- many $ satisfy (\x -> x /= '\'')
    let contents'' = contents ++ "'" ++ contents'
    string "'"
    (try $ stringTail sentinel contents'') <|> (stringCont sentinel contents'')

stringTail sentinel contents = do
    string sentinel
    string "'"
    return $ StrLit contents

--
-- Reference or Application
--

refOrApp pctx = do
    n <- name
    application pctx n <|> return (ValueOf n)

application pctx n = do
    keyword "("
    actuals <- sepBy (expr pctx) (keyword ",")
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

intLit = do
    ds <- many1 digit
    spaces
    return (IntLit (read ds))

--
-- Driver
--

parseExpr text = parse (expr (ParseContext 0)) "" text
