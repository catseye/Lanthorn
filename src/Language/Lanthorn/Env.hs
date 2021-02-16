module Language.Lanthorn.Env where

import qualified Data.Map.Strict as Map

import Language.Lanthorn.Value

type Env = Map.Map String Value


empty = Map.empty

lookup name env = Map.lookup name env

extend [] env = env
extend ((name, value):rest) env =
    case (Map.lookup name env) of
        Just existing -> error ("Already defined: " ++ name)
        Nothing -> extend rest $ Map.insert name value env


stdEnv = extend
  [
    ("true", Boolean True),
    ("false", Boolean False),
    ("sub", Function (\[Number a, Number b] -> Number (a - b))),
    ("mul", Function (\[Number a, Number b] -> Number (a * b))),
    ("eq", Function (\[a, b] -> Boolean (a == b)))
  ] empty
