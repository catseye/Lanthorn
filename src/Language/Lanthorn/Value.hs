module Language.Lanthorn.Value where

data Value = Number Integer
           | Boolean Bool
           | Function ([Value] -> Value)

instance Show Value where
    show (Number n)      = (show n)
    show (Boolean True)  = "#t"
    show (Boolean False) = "#f"
    show (Function f)    = "<<function>>"

instance Eq Value where
    (Number n1) == (Number n2) = n1 == n2
    (Boolean b1) == (Boolean b2) = b1 == b2
    _ == _ = False
