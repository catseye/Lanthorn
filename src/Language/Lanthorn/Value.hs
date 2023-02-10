module Language.Lanthorn.Value where

import Language.Lanthorn.AST

data Value = Number Integer
           | Boolean Bool
           | Function ([Value] -> Value)
           | Syntax Expr
           | StringV Name
           | ListV [Value]

instance Show Value where
    show (Number n)      = (show n)
    show (Boolean True)  = "true"
    show (Boolean False) = "false"
    show (Function _)    = "<<function>>"
    show (Syntax _)      = "<<syntax>>"
    show (StringV s)     = (show s)
    show (ListV l)       = (show l)

instance Eq Value where
    (Number n1) == (Number n2) = n1 == n2
    (Boolean b1) == (Boolean b2) = b1 == b2
    (Syntax e1) == (Syntax e2) = e1 == e2
    (StringV s1) == (StringV s2) = s1 == s2
    (ListV l1) == (ListV l2) = l1 == l2
    _ == _ = False
