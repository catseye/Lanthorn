module Language.Lanthorn.LetRec2 where

import Language.Lanthorn.AST


convert ast =
    let
        wrapperNameOuter name = name ++ "$0"
        wrapperNameInner name = name ++ "$1"
        createLocalBindings injecteds allInjectedNames =
            let
                createLocalBinding (injectedName, formals) =
                    let
                        formals' = map (wrapperNameInner) formals
                        actuals = map (ValueOf) (formals' ++ (map (wrapperNameInner) allInjectedNames))
                    in
                        (injectedName, Fun formals' (Apply (wrapperNameInner injectedName) actuals))
            in
                map createLocalBinding injecteds
        createEnrichedBindings bindings injecteds =
            let
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
            in
                map createEnrichedBinding bindings
        createWrapperBindings bindings injecteds =
            let
                createWrapperBinding acc binding =
                    case binding of
                        (name, (Fun formals body)) ->
                            let
                                actuals = map (ValueOf) (formals ++ (map (\x -> wrapperNameOuter $ fst x) injecteds))
                                expr' = Fun formals (Apply (wrapperNameOuter name) actuals)
                            in
                                acc ++ [(name, expr')]
                        other ->
                            acc
            in
                foldl createWrapperBinding [] bindings
    in
        case ast of
            (Fun formals body) -> Fun formals (convert body)
            (Apply name args) -> Apply name (map (convert) args)
            (LetRec bindings body) ->
                let
                    bindings' = map (\(name, expr) -> (name, (convert expr))) bindings
                    body' = convert body
                    injecteds = map (\(name, (Fun formals _)) -> (name, formals)) bindings'
                    enrichedBindings = createEnrichedBindings bindings' injecteds
                    wrapperBindings = createWrapperBindings bindings' injecteds
                in
                    LetStar (enrichedBindings ++ wrapperBindings) body'
            (If c t f) -> If (convert c) (convert t) (convert f)
            (LetStar bindings body) -> LetStar (map (\(name, expr) -> (name, (convert expr))) bindings) (convert body)
            other -> other
