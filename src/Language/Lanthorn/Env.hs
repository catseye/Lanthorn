module Language.Lanthorn.Env where

import Language.Lanthorn.Value

type Env = [(String, Value)]


empty = []

fetch name [] = Nothing
fetch name ((name', value):rest) =
   if name == name' then Just value else fetch name rest

extend bindings env = bindings ++ env


stdEnv = extend
  [
    ("true", Boolean True),
    ("false", Boolean False),
    ("add", Function (\[Number a, Number b] -> Number (a + b))),
    ("sub", Function (\[Number a, Number b] -> Number (a - b))),
    ("mul", Function (\[Number a, Number b] -> Number (a * b))),
    ("eq", Function (\[a, b] -> Boolean (a == b)))
  ] empty
