module Language.Lanthorn.Pretty where

import Language.Lanthorn.AST


indent 0 = ""
indent i = "  " ++ (indent $ i - 1)

pretty i (Fun formals expr) =
    "fun(" ++ (prettyFormals i formals) ++ ") -> " ++ pretty (i+1) expr
pretty i (Apply name actuals) =
    name ++ "(" ++ (prettyExprList i actuals) ++ ")"
pretty i (LetRec bindings expr) =
    "letrec\n" ++ (prettyBindings (i+1) bindings) ++ (indent i) ++ "in\n" ++ (indent (i+1)) ++ (pretty (i+1) expr)
pretty i (LetStar bindings expr) =
    "let\n" ++ (prettyBindings (i+1) bindings) ++ (indent i) ++ "in\n" ++ (indent (i+1)) ++ (pretty (i+1) expr)
pretty i (If testExpr trueExpr falseExpr) =
    "if " ++ (pretty (i+1) testExpr) ++ " then " ++ (pretty (i+1) trueExpr) ++ " else " ++ (pretty (i+1) falseExpr)
pretty i (NumLit n) = show n
pretty i (ValueOf n) = n
pretty i (Quoted expr) =
    "<< " ++ pretty (i+1) expr ++ " >>"
pretty i (List exprs) =
    "[" ++ (prettyExprList i exprs) ++ "]"

prettyFormals i [] = ""
prettyFormals i [name] = name
prettyFormals i (name:rest) = name ++ ", " ++ (prettyFormals i rest)

prettyExprList i [] = ""
prettyExprList i [expr] = pretty i expr
prettyExprList i (expr:rest) = (pretty i expr) ++ ", " ++ (prettyExprList i rest)

prettyBindings i [] = ""
prettyBindings i ((name, expr):rest) = (indent i) ++ name ++ " = " ++ (pretty i expr) ++ "\n" ++ prettyBindings i rest
