module Language.Lanthorn.Pretty where

import Language.Lanthorn.AST


indent 0 = ""
indent i = "  " ++ (indent $ i - 1)

pretty i (Fun formals expr) =
    "fun(" ++ (prettyNames i formals) ++ ") -> " ++ pretty (i+1) expr
pretty i (Apply name actuals) =
    name ++ "(" ++ (prettyExprList i actuals) ++ ")"
pretty i (LetRec bindings expr) =
    "letrec\n" ++ (prettyBindings (i+1) bindings) ++ (indent i) ++ "in\n" ++ (indent (i+1)) ++ (pretty (i+1) expr)
pretty i (LetStar bindings expr) =
    "let\n" ++ (prettyBindings (i+1) bindings) ++ (indent i) ++ "in\n" ++ (indent (i+1)) ++ (pretty (i+1) expr)
pretty i (If testExpr trueExpr falseExpr) =
    "if " ++ (pretty (i+1) testExpr) ++ " then " ++ (pretty (i+1) trueExpr) ++ " else " ++ (pretty (i+1) falseExpr)
pretty i (IntLit n) = show n
pretty i (StrLit s) = "''" ++ s ++ "''"
pretty i (Import importfroms expr) =
    "import\n" ++ (prettyImportFroms (i+1) importfroms) ++ (indent i) ++ "in\n" ++ (indent (i+1)) ++ (pretty (i+1) expr)
pretty i (Export names) = prettyNames i names
pretty i (ValueOf n) = n

prettyNames i [] = ""
prettyNames i [name] = name
prettyNames i (name:rest) = name ++ ", " ++ (prettyNames i rest)

prettyExprList i [] = ""
prettyExprList i [expr] = pretty i expr
prettyExprList i (expr:rest) = (pretty i expr) ++ ", " ++ (prettyExprList i rest)

prettyBindings i [] = ""
prettyBindings i ((name, expr):rest) = (indent i) ++ name ++ " = " ++ (pretty i expr) ++ "\n" ++ prettyBindings i rest

prettyImportFroms i [] = ""
prettyImportFroms i ((ImportFrom importspecs modulespec):rest) = (indent i) ++ (prettyImportSpecs i importspecs) ++ " from " ++ (show modulespec) ++ "\n" ++ prettyImportFroms i rest

prettyImportSpecs i [] = ""
prettyImportSpecs i [(ImportSpec n nn)] = n
prettyImportSpecs i ((ImportSpec n nn):rest) = n ++ ", " ++ (prettyImportSpecs i rest)
