module Language.Lanthorn.Eval where

import Language.Lanthorn.AST
import qualified Language.Lanthorn.Env as Env
import Language.Lanthorn.Value (Value(Boolean, Function, Number, Syntax))


unsyntax [Syntax e, Function apply_, Function let_, Function if_, Function valueof_, Function numlit_, Function syntax_, Function fun_] =
    -- To do this properly, we need a "Name" type value at the object level (i.e. a string).
    case e of
        Apply name exprs ->
            apply_ (map (Syntax) exprs)
        LetStar bindings expr ->
            -- TODO: map out bindings
            let_ [Syntax expr]
        If cexpr texpr fexpr ->
            if_ [(Syntax cexpr), (Syntax texpr), (Syntax fexpr)]
        ValueOf n ->
            valueof_ [] -- TODO: string
        NumLit i ->
            numlit_ [Number i]
        Quoted e ->
            syntax_ [Syntax e]
        Fun names expr ->
            fun_ [Syntax expr]


stdEnv = Env.extend
  [
    ("true", Boolean True),
    ("false", Boolean False),
    ("add", Function (\[Number a, Number b] -> Number (a + b))),
    ("sub", Function (\[Number a, Number b] -> Number (a - b))),
    ("mul", Function (\[Number a, Number b] -> Number (a * b))),
    ("eq", Function (\[a, b] -> Boolean (a == b))),
    ("eval", Function (\[Syntax e] -> evalExpr stdEnv e)),
    ("unsyntax", Function unsyntax)
  ] Env.empty

evalTopLevelExpr :: Expr -> Either String Value
evalTopLevelExpr expr =
    Right (evalExpr stdEnv expr)

--
-- Evaluator
--

evalExpr env (NumLit i) = Number i

evalExpr env (ValueOf name) = case Env.lookup name env of
    Just value -> value
    Nothing -> error ("Not in scope: " ++ name ++ " (env: " ++ (show env) ++ ")")

evalExpr env (Quoted e) = Syntax e

evalExpr env (LetStar [] body) = evalExpr env body
evalExpr env (LetStar ((name, expr):rest) body) =
    let
        val = evalExpr env expr
        env' = Env.extend [(name,val)] env
    in
        evalExpr env' (LetStar rest body)

evalExpr env (If c t f) =
    case evalExpr env c of
        Boolean True -> evalExpr env t
        Boolean False -> evalExpr env f
        other -> error ("Expected boolean: " ++ show other)

evalExpr env (Fun formals body) =
    let
        f values =
           let
               env' = Env.extend (zip formals values) env
           in
               evalExpr env' body
    in
        Function f

evalExpr env (Apply name actualExprs) =
    case (evalExpr env (ValueOf name)) of
        Function f ->
            let
                actuals = map (\expr -> evalExpr env expr) actualExprs
            in
                f actuals
        other -> error ("Expected function: " ++ show other)

evalExpr env other = error ("Unimplemented: " ++ show other)
