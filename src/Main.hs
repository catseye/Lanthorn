module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Lanthorn.AST as AST
import qualified Language.Lanthorn.Pretty as Pretty
import qualified Language.Lanthorn.Parser as Parser
import qualified Language.Lanthorn.LetRec2 as LetRec
import qualified Language.Lanthorn.Eval as Eval


main = do
    args <- getArgs
    case args of
        ["pretty", fileName] -> do
            expr <- loadSource fileName
            putStrLn $ Pretty.pretty 0 expr
        ["desugar", fileName] -> do
            expr <- loadSource fileName
            putStrLn $ Pretty.pretty 0 $ LetRec.convert expr
        ["eval", fileName] -> do
            expr <- loadSource fileName
            case Eval.evalTopLevelExpr $ LetRec.convert expr of
                Right value ->
                    putStrLn $ show value
                Left error ->
                    abortWith $ show error
        _ -> do
            abortWith "Usage: lanthorn (pretty|desugar|eval) <input-filename>"

loadSource fileName = do
    text <- readFile fileName
    case Parser.parseExpr text of
        Right expr -> do
            return expr
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)
