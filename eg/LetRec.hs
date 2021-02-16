--
-- In languages like Haskell, you get letrec behaviour "for free"
-- because the bindings of the names are resolved lazily.
--

testIt =
    let
        oddp  = \x -> if x == 0 then False else evenp (x - 1)
        evenp = \x -> if x == 0 then True else oddp (x - 1)
    in
        evenp 6
