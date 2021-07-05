module Language.Lanthorn.LetRec where

import Language.Lanthorn.AST
import Language.Lanthorn.Pretty


convert (Fun formals body) = Fun formals (convert body)
convert (Apply name args) = Apply name (map (convert) args)
convert (LetRec bindings body) = convertToLetStar (convertBindings bindings) (convert body)
convert (If c t f) = If (convert c) (convert t) (convert f)
convert (LetStar bindings body) = LetStar (convertBindings bindings) (convert body)
convert other = other

convertBindings :: [(String, Expr)] -> [(String, Expr)]
convertBindings [] = []
convertBindings ((name, expr):rest) = ((name, (convert expr)):(convertBindings rest))

convertArms [] = []
convertArms ((ante, cons):rest) = (((convert ante), (convert cons)):(convertArms rest))

convertToLetStar :: [(String, Expr)] -> Expr -> Expr
convertToLetStar bindings body =
    let
        -- For each binding, we need to send down the relevant parts of all
        -- the bindings in the letrec, so it can compose the recursive call.
        -- The relevant parts are the *name* of each binding and its *formals*.
        -- We call such a pair an "injected", for no terribly good reason
        -- (possibly because it is "injected" into every binding in the letrec).
        getInjected (name, (Fun formals body)) = (name, formals)
        injecteds = map (getInjected) bindings
        enrichedBindings = createEnrichedBindings bindings injecteds
        wrapperBindings = createWrapperBindings bindings injecteds
    in
        LetStar (enrichedBindings ++ wrapperBindings) body

wrapperNameOuter name = name ++ "0"  -- TODO: more hygenic!
wrapperNameInner name = name ++ "1"

createEnrichedBindings [] injecteds = []
createEnrichedBindings (binding@(name, (Fun formals body)):rest) injecteds =
    let
        name' = wrapperNameOuter name
        injectedNames = map (fst) injecteds
        formals' = formals ++ (map (wrapperNameInner) injectedNames)
        body' = (LetStar (createLocalBindings injecteds injectedNames) body)
        expr' = (Fun formals' body')
        binding = (name', expr')
    in
        (binding:createEnrichedBindings rest injecteds)
createEnrichedBindings (binding:rest) injecteds =
    (binding:createEnrichedBindings rest injecteds)

createLocalBindings [] _ = []
createLocalBindings (injected@(injectedName, formals):injecteds) allInjectedNames =
    let
        formals' = map (wrapperNameInner) formals
        actuals = map (ValueOf) (formals' ++ (map (wrapperNameInner) allInjectedNames))
        binding = (injectedName, Fun formals' (Apply (wrapperNameInner injectedName) actuals))
    in
        (binding:createLocalBindings injecteds allInjectedNames)

createWrapperBindings [] injecteds = []
createWrapperBindings ((name, (Fun formals body)):rest) injecteds =
    let
        name' = name
        actuals = map (ValueOf) (formals ++ (map (\x -> wrapperNameOuter $ fst x) injecteds))
        expr' = Fun formals (Apply (wrapperNameOuter name) actuals)
        binding = (name', expr')
    in
        (binding:createWrapperBindings rest injecteds)
createWrapperBindings (binding:rest) injecteds =
    createWrapperBindings rest injecteds
