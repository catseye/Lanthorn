module Language.Lanthorn.LetRec2 where

import Language.Lanthorn.AST


convert (Fun formals body) = Fun formals (convert body)
convert (Apply name args) = Apply name (map (convert) args)
convert (LetRec bindings body) =
    let
        bindings' = convertBindings bindings
        body' = convert body
        injecteds = map (\(name, (Fun formals _)) -> (name, formals)) bindings'
        enrichedBindings = createEnrichedBindings bindings' injecteds
        wrapperBindings = createWrapperBindings bindings' injecteds
    in
        LetStar (enrichedBindings ++ wrapperBindings) body'
convert (If c t f) = If (convert c) (convert t) (convert f)
convert (LetStar bindings body) = LetStar (convertBindings bindings) (convert body)
convert other = other

convertBindings = map (\(name, expr) -> (name, (convert expr)))
convertArms = map (\((ante, cons):rest) -> ((convert ante), (convert cons)))

--

wrapperNameOuter name = name ++ "$0"
wrapperNameInner name = name ++ "$1"

createEnrichedBindings bindings injecteds =
    map createEnrichedBinding bindings where
        createEnrichedBinding binding =
            case binding of
                (name, (Fun formals body)) ->
                    let
                        name' = wrapperNameOuter name
                        injectedNames = map (fst) injecteds
                        formals' = formals ++ (map (wrapperNameInner) injectedNames)
                        body' = (LetStar (createLocalBindings injecteds injectedNames) body)
                        expr' = (Fun formals' body')
                    in
                        (name', expr')
                other ->
                    other

createLocalBindings injecteds allInjectedNames =
    map createLocalBinding injecteds where
        createLocalBinding njected@(injectedName, formals) =
            let
                formals' = map (wrapperNameInner) formals
                actuals = map (ValueOf) (formals' ++ (map (wrapperNameInner) allInjectedNames))
            in
                (injectedName, Fun formals' (Apply (wrapperNameInner injectedName) actuals))

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