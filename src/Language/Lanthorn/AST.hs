module Language.Lanthorn.AST where

type Name = String

data Expr = Fun [Name] Expr
          | Apply Name [Expr]
          | LetRec [(Name, Expr)] Expr        -- will be rewritten away before evaluation.
          | LetStar [(Name, Expr)] Expr
          | If Expr Expr Expr
          | IntLit Integer
          | StrLit [Char]
          | Import [ImportFrom] Expr
          | Export [Name]
          | ValueOf Name
    deriving (Show, Ord, Eq)

data ImportFrom = ImportFrom [ImportSpec] ModuleSpec
    deriving (Show, Ord, Eq)

data ImportSpec = ImportSpec Name (Maybe Name)
    deriving (Show, Ord, Eq)

data ModuleSpec = Builtins
                | ModuleSpec Name
    deriving (Show, Ord, Eq)
