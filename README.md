Lanthorn
========

_Proof of Concept_
| _See also:_ [Lariat](https://github.com/catseye/Lariat#readme)
∘ [Iphigeneia](https://github.com/catseye/Iphigeneia#readme)

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
Here is what it produces:

    letrec
        odd  = fun(x) -> if eq(x, 0) then false else even(sub(x, 1))
        even = fun(x) -> if eq(x, 0) then true else odd(sub(x, 1))
    in
        even(6)
    => let
    =>   odd0 = fun(x, odd1, even1) -> let
    =>       odd = fun(x1) -> odd1(x1, odd1, even1)
    =>       even = fun(x1) -> even1(x1, odd1, even1)
    =>     in
    =>       if eq(x, 0) then false else even(sub(x, 1))
    =>   even0 = fun(x, odd1, even1) -> let
    =>       odd = fun(x1) -> odd1(x1, odd1, even1)
    =>       even = fun(x1) -> even1(x1, odd1, even1)
    =>     in
    =>       if eq(x, 0) then true else odd(sub(x, 1))
    =>   odd = fun(x) -> odd0(x, odd0, even0)
    =>   even = fun(x) -> even0(x, odd0, even0)
    => in
    =>   even(6)

In English, it adds a number of extra parameters to each function in
the set of bindings.  Specifically, it adds one parameter for each
of the bindings.  It then sets up some bindings _inside_ each function
so that the function uses these parameters for the recursive calls
it makes.  It also sets up some bindings outside of these functions
to that the body of the `letrec` sees functions with the original
parameters they had, hiding all these extra parameters.

TODO
----

*   The implementation of the transformation isn't fully general yet.
    It needs to handle `let` inside the definitions and the body of a
    `let`.
*   The transformation should make more effort at name mangling
    hygiene.
*   The transformation should retain the names of the original
    arguments of the functions.
*   There needs to be a test confirming that it can handle multiple
    arguments in the original functions.

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
        factopen = fun(f, n) -> if eq(n, 1) then 1 else f(f, sub(n, 1))
        target = 7
    in
        letrec
            oddp  = fun(x) -> if eq(x, 0) then false else evenp(sub(x, 1))
            evenp = fun(x) -> if eq(x, 0) then true else oddp(sub(x, 1))
        in
            if oddp(target) then factopen(factopen, target) else 0
    ===> 105
