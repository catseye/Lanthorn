module Language.Lanthorn.Eval where

import Language.Lanthorn.AST
import qualified Language.Lanthorn.Env as Env
import qualified Language.Lanthorn.Value as Value


evalTopLevelExpr :: Expr -> Either String Value.Value
evalTopLevelExpr expr =
    Right (evalExpr Env.stdEnv expr)

--
-- Evaluator
--

evalExpr env (IntLit i) = Value.Number i

evalExpr env (ValueOf name) = case Env.lookup name env of
    Just value -> value
    Nothing -> error ("Not in scope: " ++ name ++ " (env: " ++ (show env) ++ ")")

evalExpr env (LetStar [] body) = evalExpr env body
evalExpr env (LetStar ((name, expr):rest) body) =
    let
        val = evalExpr env expr
        env' = Env.extend [(name,val)] env
    in
        evalExpr env' (LetStar rest body)

evalExpr env (If c t f) =
    case evalExpr env c of
        Value.Boolean True -> evalExpr env t
        Value.Boolean False -> evalExpr env f
        other -> error ("Expected boolean: " ++ show other)

evalExpr env (Fun formals body) =
    let
        f values =
           let
               env' = Env.extend (zip formals values) env
           in
               evalExpr env' body
    in
        Value.Function f

evalExpr env (Apply name actualExprs) =
    case (evalExpr env (ValueOf name)) of
        (Value.Function f) ->
            let
                actuals = map (\expr -> evalExpr env expr) actualExprs
            in
                f actuals
        other -> error ("Expected function: " ++ show other)
