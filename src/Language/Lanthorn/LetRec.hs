module Language.Lanthorn.LetRec where

import Language.Lanthorn.AST


convert (Fun formals body) = Fun formals (convert body)
convert (Apply name args) = Apply name (map (convert) args)
convert (LetRec bindings body) = convertToLetStar (convertBindings bindings) (convert body)
convert (If c t f) = If (convert c) (convert t) (convert f)
convert other = other   -- TODO: handle LetStar!

convertBindings :: [(String, Expr)] -> [(String, Expr)]
convertBindings [] = []
convertBindings ((name, expr):rest) = ((name, (convert expr)):(convertBindings rest))

convertArms [] = []
convertArms ((ante, cons):rest) = (((convert ante), (convert cons)):(convertArms rest))

convertToLetStar :: [(String, Expr)] -> Expr -> Expr
convertToLetStar bindings body =
    let
        injecteds = map (fst) bindings
        enrichedBindings = createEnrichedBindings bindings injecteds
        wrapperBindings = createWrapperBindings bindings injecteds
    in
        LetStar (enrichedBindings ++ wrapperBindings) body

wrapperNameOuter name = name ++ "0"  -- TODO: more hygenic!
wrapperNameInner name = name ++ "1"

createEnrichedBindings [] injecteds = []
createEnrichedBindings ((name, (Fun formals body)):rest) injecteds =
    let
        name' = wrapperNameOuter name
        formals' = formals ++ (map (wrapperNameInner) injecteds)
        body' = (LetStar (createLocalBindings injecteds injecteds) body)
        expr' = (Fun formals' body')
        binding = (name', expr')
    in
        (binding:createEnrichedBindings rest injecteds)
createEnrichedBindings (binding:rest) injecteds =
    (binding:createEnrichedBindings rest injecteds)

createLocalBindings [] _ = []
createLocalBindings (injected:injecteds) allInjecteds =
    let
        -- FIXME use the real formals of each injected identifier! also, allow shadowing!
        actuals = map (ValueOf) (["x1"] ++ (map (wrapperNameInner) allInjecteds))
        binding = (injected, Fun ["x1"] (Apply (wrapperNameInner injected) actuals))
    in
        (binding:createLocalBindings injecteds allInjecteds)

createWrapperBindings [] injecteds = []
createWrapperBindings ((name, (Fun formals body)):rest) injecteds =
    let
        name' = name
        actuals = map (ValueOf) (formals ++ (map (wrapperNameOuter) injecteds))
        expr' = Fun formals (Apply (wrapperNameOuter name) actuals)
        binding = (name', expr')
    in
        (binding:createWrapperBindings rest injecteds)
createWrapperBindings (binding:rest) injecteds =
    createWrapperBindings rest injecteds
