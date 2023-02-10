module Language.Lanthorn.AST where

type Name = String

data Expr = Fun [Name] Expr
          | Apply Name [Expr]
          | LetRec [(Name, Expr)] Expr        -- will be rewritten away before evaluation.
          | LetStar [(Name, Expr)] Expr
          | If Expr Expr Expr
          | ListExpr [Expr]
          | NumLit Integer
          | ValueOf Name
          | Quoted Expr
    deriving (Show, Ord, Eq)
