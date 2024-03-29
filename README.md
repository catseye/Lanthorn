Lanthorn
========

Version 1.0 | _Entry_ @ [catseye.tc](https://catseye.tc/node/Lanthorn)
| _See also:_ [Lariat](https://catseye.tc/node/Lariat)
* [Iphigeneia](https://catseye.tc/node/Iphigeneia)

When I first came across a explanation of how `letrec` works, it was
in terms of updating references: each of the names is bound to a cell,
and when the thing that name refers to is eventually defined, that cell
is updated with that thing.

My reaction to this was _ugh_.  I mean, sure, it works, but in the
context of functional programming, such an imperative description is
really unsatisfying.

So, I present here a tiny, eager, purely functional language, christened
**Lanthorn**, whose sole purpose is to host a demonstration of how `letrec`
can be written as syntactic sugar over `let` in a purely functional way.

The transformation is unobtrusive in that it doesn't make any changes in
the body of the `letrec`.  The resulting code is not, however, intended
to be efficient.

Since the language is simple enough and conventional enough that you
can probably guess what the programs mean, let's leave the description
of the language until [Appendix A](#appendix-a), and go straight into
describing the transformation.

Desugaring
----------

    -> Tests for functionality "Desugar Lanthorn Program"

Basically, what we want to do, is take this...

    letrec
        odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
        even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
    in
        even(6)

...and turn it into this.

    let
        odd0  = fun(x, odd1, even1) ->
                     let
                         odd = fun(x) -> odd1(x, odd1, even1)
                         even = fun(x) -> even1(x, odd1, even1)
                     in
                         if eq(x, 0) then false else even(sub(x, 1)))
        even0 = fun(x, odd1, even1) ->
                     let
                         odd = fun(x) -> odd1(x, odd1, even1)
                         even = fun(x) -> even1(x, odd1, even1)
                     in
                         if eq(x, 0) then true else odd(sub(x, 1)))
        odd   = fun(x) -> odd0(x, odd0, even0)
        even  = fun(x) -> even0(x, odd0, even0)
    in
        even(6)

Our evaluator implements this transformation in the
[Language.Lanthorn.LetRec](src/Language/Lanthorn/LetRec.hs) module.
Here is what it produces.  Note there is a bit of name-mangling
added, compared to the hand-written expansion above.

    letrec
        odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
        even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
    in
        even(6)
    => let
    =>   odd$0 = fun(x, odd$1, even$1) -> let
    =>       odd = fun(x$1) -> odd$1(x$1, odd$1, even$1)
    =>       even = fun(x$1) -> even$1(x$1, odd$1, even$1)
    =>     in
    =>       if eq(x, 0) then false else even(sub(x, 1))
    =>   even$0 = fun(x, odd$1, even$1) -> let
    =>       odd = fun(x$1) -> odd$1(x$1, odd$1, even$1)
    =>       even = fun(x$1) -> even$1(x$1, odd$1, even$1)
    =>     in
    =>       if eq(x, 0) then true else odd(sub(x, 1))
    =>   odd = fun(x) -> odd$0(x, odd$0, even$0)
    =>   even = fun(x) -> even$0(x, odd$0, even$0)
    => in
    =>   even(6)

In English, it adds a number of extra parameters to each function in
the set of bindings.  Specifically, it adds one parameter for each
of the bindings.  It then sets up some bindings _inside_ each function
so that the function uses these parameters for the recursive calls
it makes.  It also sets up some bindings outside of these functions
to that the body of the `letrec` sees functions with the original
parameters they had, hiding all these extra parameters.

Related Work
------------

Xavier Pinho has written up an alternative way of transforming `letrec`
into `let`, using surjective pairing and the Y combinator, in
[an issue on the Lanthorn project on GitHub](https://github.com/catseye/Lanthorn/issues/1).

Appendix A
----------

### Basic Syntax of Lanthorn

    -> Tests for functionality "Pretty-print Lanthorn Program"

Function application, numeric literals, string literals.

    add(1, 2)
    => add(1, 2)

Name binding (`let`) and name reference.

    let a = 1
        b = 1
        in zed(a, b)
    => let
    =>   a = 1
    =>   b = 1
    => in
    =>   zed(a, b)

The character `$` may not appear in user-supplied names.

    let
      a$b = 1
    in
      zed(a$b)
    ?> unexpected "$"

Conditional by boolean (`if`).

    if gt(a, b) then a else b
    => if gt(a, b) then a else b

Function values.

    let up = fun(x) -> add(x, 1) in up(5)
    => let
    =>   up = fun(x) -> add(x, 1)
    => in
    =>   up(5)

### Basic Semantics of Lanthorn

    -> Tests for functionality "Evaluate Lanthorn Program"

    1
    ===> 1

    if true then 5 else 6
    ===> 5

    let a = 2 in a
    ===> 2

Basic functions.

    let r = fun(x) -> 77 in r(1)
    ===> 77

    let r = fun(x) -> x in r(66)
    ===> 66

`let` is like Scheme's `let*` or Standard ML's `let`:
later bindings can see earlier bindings.

    let
      p = 99
      r = fun(x) -> p
    in
      r(66)
    ===> 99

Note that depicting a function is implementation-dependent.

    fun(x) -> x
    ===> <<function>>

Can't shadow a binding in `let`.

    let a = 1 in let a = 2 in a
    ???> Already defined: a

Can't shadow a binding in the formals of a `fun`.

    let r = fun(x, x) -> x in r(10, 10)
    ???> Already defined: x

    let r = fun(x) -> let x = 3 in x in r(10)
    ???> Already defined: x

#### `letrec`

Basic usage of `letrec`.

    letrec
        oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
        evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
    in
        evenp(6)
    ===> true

    letrec
        oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
        evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
    in
        evenp(5)
    ===> false

`letrec` nested inside an `if` inside a function definition in an arm of
another `letrec`.

    letrec
        facto = fun(n) -> if eq(n, 1) then 1 else
            letrec
                oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
                evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
            in
                if oddp(n) then
                    mul(n, facto(sub(n, 1)))
                else
                    facto(sub(n, 1))
    in
        facto(8)
    ===> 105

`letrec` nested in the body of another `letrec`.

    letrec
        oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
        evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
    in
        letrec facto = fun(n) ->
            if eq(n, 1) then
                1
            else if oddp(n) then
                mul(n, facto(sub(n, 1)))
            else
                facto(sub(n, 1))
    in
        facto(8)
    ===> 105

Nested `letrec`, nested right in the arm of another `letrec`.  Currently,
this is an error, because the inner scope cannot "see" the outer `letrec`.
Though I'm not yet convinced of what the most reasonable behaviour is here.

    letrec
        facto =
            letrec
                oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
                evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
            in
                fun(n) -> if eq(n, 1) then 1 else
                    if oddp(n) then
                        mul(n, facto(sub(n, 1)))
                    else
                        facto(sub(n, 1))
    in
        facto(8)
    ???> Not in scope: facto

`letrec` nested inside a function definition inside an arm of a plain `let`.

    let
        factoo = fun(f, n) ->
            letrec
                oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
                evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
            in
                if eq(n, 1) then 1 else
                    if oddp(n) then
                        mul(n, f(f, sub(n, 1)))
                    else
                        f(f, sub(n, 1))
    in
        factoo(factoo, 7)
    ===> 105

`letrec` nested inside body of a plain `let`.

    let
        factopen = fun(f, n) -> if eq(n, 1) then 1 else mul(n, f(f, sub(n, 1)))
        target = 7
    in
        letrec
            oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
            evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
        in
            if oddp(target) then factopen(factopen, target) else 0
    ===> 5040

`letrec` works on functions that have more than one argument.

    letrec
        oddsump  = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), y, z)
        evensump = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then true else oddsump(sub(x, 1), y, z)
    in
        evensump(5,3,1)
    ===> false

    letrec
        oddsump  = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), y, z)
        evensump = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then true else oddsump(sub(x, 1), y, z)
    in
        evensump(6,3,1)
    ===> true

`letrec` works on functions which use different argument names.

    letrec
        oddsump  = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), y, z)
        evensump = fun(p,q,r) -> if eq(add(p, add(q, r)), add(q, r)) then true else oddsump(sub(p, 1), q, r)
    in
        evensump(5,3,1)
    ===> false

    letrec
        oddsump  = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), y, z)
        evensump = fun(p,q,r) -> if eq(add(p, add(q, r)), add(q, r)) then true else oddsump(sub(p, 1), q, r)
    in
        evensump(6,3,1)
    ===> true

`letrec` works on functions that have differing numbers of arguments.

    letrec
        oddsump  = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), add(y, z))
        evensump = fun(p,q)   -> if eq(add(p, q), q) then true else oddsump(sub(p, 1), 1, sub(q, 1))
    in
        oddsump(5,3,1)
    ===> true

### Properties of the `letrec` transformation

    -> Tests for functionality "Desugar Lanthorn Program"

When a `letrec` is desugared, the generated functions have argument
names that are based on the original argument names.  Also, the
innermost `let`s bind the plain names to functions with the same arity
as the original functions.

    letrec
        oddsump  = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), y, z)
        evensump = fun(x,y,z) -> if eq(add(x, add(y, z)), add(y, z)) then true else oddsump(sub(x, 1), y, z)
    in
        evensump(5,3,1)
    => let
    =>   oddsump$0 = fun(x, y, z, oddsump$1, evensump$1) -> let
    =>       oddsump = fun(x$1, y$1, z$1) -> oddsump$1(x$1, y$1, z$1, oddsump$1, evensump$1)
    =>       evensump = fun(x$1, y$1, z$1) -> evensump$1(x$1, y$1, z$1, oddsump$1, evensump$1)
    =>     in
    =>       if eq(add(x, add(y, z)), add(y, z)) then false else evensump(sub(x, 1), y, z)
    =>   evensump$0 = fun(x, y, z, oddsump$1, evensump$1) -> let
    =>       oddsump = fun(x$1, y$1, z$1) -> oddsump$1(x$1, y$1, z$1, oddsump$1, evensump$1)
    =>       evensump = fun(x$1, y$1, z$1) -> evensump$1(x$1, y$1, z$1, oddsump$1, evensump$1)
    =>     in
    =>       if eq(add(x, add(y, z)), add(y, z)) then true else oddsump(sub(x, 1), y, z)
    =>   oddsump = fun(x, y, z) -> oddsump$0(x, y, z, oddsump$0, evensump$0)
    =>   evensump = fun(x, y, z) -> evensump$0(x, y, z, oddsump$0, evensump$0)
    => in
    =>   evensump(5, 3, 1)

The transformation mangles names that it generates so that they never
shadow names that appear in the user's program.  (Names containing `$` may
not appear in a user-supplied program.)

    let
        odd0 = fun(a, b, c) -> a
    in
        letrec
            odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
            even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
        in
            even(6)
    => let
    =>   odd0 = fun(a, b, c) -> a
    => in
    =>   let
    =>     odd$0 = fun(x, odd$1, even$1) -> let
    =>         odd = fun(x$1) -> odd$1(x$1, odd$1, even$1)
    =>         even = fun(x$1) -> even$1(x$1, odd$1, even$1)
    =>       in
    =>         if eq(x, 0) then false else even(sub(x, 1))
    =>     even$0 = fun(x, odd$1, even$1) -> let
    =>         odd = fun(x$1) -> odd$1(x$1, odd$1, even$1)
    =>         even = fun(x$1) -> even$1(x$1, odd$1, even$1)
    =>       in
    =>         if eq(x, 0) then true else odd(sub(x, 1))
    =>     odd = fun(x) -> odd$0(x, odd$0, even$0)
    =>     even = fun(x) -> even$0(x, odd$0, even$0)
    =>   in
    =>     even(6)

    -> Tests for functionality "Evaluate Lanthorn Program"

    letrec
        odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
        odd0 = fun(a, b, c) -> a
        even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
    in
        even(6)
    ===> true

You might think that instead of mangling names, we could just allow shadowing
in the language.  But that by itself doesn't solve our problem, since you
could still say something like the following.  The `letrec` desugaring would
have to be more aware of how it constructs names, at any rate, in order to
avoid the conflict here.  And mangling is the simplest way to do that.

    -> Tests for functionality "Desugar Lanthorn Program"

    letrec
        odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
        odd0 = fun(a, b, c) -> a
        even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
    in
        even(6)
    => let
    =>   odd$0 = fun(x, odd$1, odd0$1, even$1) -> let
    =>       odd = fun(x$1) -> odd$1(x$1, odd$1, odd0$1, even$1)
    =>       odd0 = fun(a$1, b$1, c$1) -> odd0$1(a$1, b$1, c$1, odd$1, odd0$1, even$1)
    =>       even = fun(x$1) -> even$1(x$1, odd$1, odd0$1, even$1)
    =>     in
    =>       if eq(x, 0) then false else even(sub(x, 1))
    =>   odd0$0 = fun(a, b, c, odd$1, odd0$1, even$1) -> let
    =>       odd = fun(x$1) -> odd$1(x$1, odd$1, odd0$1, even$1)
    =>       odd0 = fun(a$1, b$1, c$1) -> odd0$1(a$1, b$1, c$1, odd$1, odd0$1, even$1)
    =>       even = fun(x$1) -> even$1(x$1, odd$1, odd0$1, even$1)
    =>     in
    =>       a
    =>   even$0 = fun(x, odd$1, odd0$1, even$1) -> let
    =>       odd = fun(x$1) -> odd$1(x$1, odd$1, odd0$1, even$1)
    =>       odd0 = fun(a$1, b$1, c$1) -> odd0$1(a$1, b$1, c$1, odd$1, odd0$1, even$1)
    =>       even = fun(x$1) -> even$1(x$1, odd$1, odd0$1, even$1)
    =>     in
    =>       if eq(x, 0) then true else odd(sub(x, 1))
    =>   odd = fun(x) -> odd$0(x, odd$0, odd0$0, even$0)
    =>   odd0 = fun(a, b, c) -> odd0$0(a, b, c, odd$0, odd0$0, even$0)
    =>   even = fun(x) -> even$0(x, odd$0, odd0$0, even$0)
    => in
    =>   even(6)

    -> Tests for functionality "Evaluate Lanthorn Program"

    let
        odd0 = fun(a, b, c) -> a
    in
        letrec
            odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
            even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
        in
            even(6)
    ===> true

Note that there is probably a case where a `letrec` nested another `letrec`, and
which shadows variables of the enclosing `letrec`, produces a less readable
error message about shadowing, because it mentions the mangled names; but
I can live with that for now.
