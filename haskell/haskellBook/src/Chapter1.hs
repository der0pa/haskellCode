--notes from chapter one of the haskell book.



--lambda calculus is the foundation of most/all FP.
-- haskell is build on typed lambda calculus. it is 'pure'.

-- f(x) = x + 1
-- passing 2 as arg becomes
-- f(2) = 2 + 1
-- becomes:
-- f(2) = 3

-- lambda calculus has 3 basic components or lambda terms:
-- 1) expressions == superet of all three things: 1,2,3
-- 2) variables == names for inputs to functions
-- 3) abstractions == function

-- functions have two parts 1) head 2) body
-- 1) \(lambda) followed by a variable == \x.
-- 2) body is after (.)  in \x.x would be x after dot.
-- lambdas are anonymous functions // no name.

-- Alpha equivalence : \x.x , \d.d , \z.z all have same meaning

-- Beta reduction : apply a function to an argument- substitute the input expression for all instances of bound vars within body. also eliminate the head.

-- (\x.x) 2 == apply func to 2 returns 2 == the identity function.
-- (\x.x) 3 == apply func to 3 returns 3 ... etc.
-- (\x.x+1) 2 == apply func to 2 returns 3 .. etc.

-- we can also apply our identity function to another lambda
-- (\x.x)(\y.y) we sub our entire abstraction (\y.y) in for x.
-- we write that like [x:=z] z is the func (\y.y)
-- so beta reduction of this is:
-- (\x.x)(\y.y)
-- [x:=(\y.y)]  applied and head dropped reduces to:
--      \y.y    another identity function

-- one more  example with an added argument:
-- (\x.x)(\y.y)z
-- lambda calculus are 'left associative'  so the above can be rewritten as:
-- ((\x.x)(\y.y))z
-- reduced onward to:
-- [x:= (\y.y)]
--      (\y.y)z  reduces and head dropped
--         [y:=z]
--             z

-- Free variables:
-- variables that are not named in the head are 'free variables'
-- so \x.xy the y in body is unbound /no y is in head so it is 'free'
-- this abstraction can be applied to an argument, z like this:
-- (\x.xy)z reduced by replacing x with z in body /head dropped:
-- (\[x:=z].xy)
-- becomes:
-- zy

-- alpha equivalence does not apply to free variables. so:
-- \x.xz and \x.xy are not alpha equivalent.
-- \xy.yx and \ab.ba are alpha equivalent.
-- \x.xz and \y.yz are also alpha equivalent.

-- 1.6 Mutiple arguments
-- Each lambda can only bind 1 parameter and can only accept 1 argument.
-- functions that require multiple args must have multiple heads.
-- this one at a time thing is called 'currying'
-- so this:   \xy.xy
-- is shorthand for this:
--            \x.(\y.xy)
-- which is two nested lambdas. one for each arg x and y
-- so the \y.xy is bound to the arg x
-- the first lambda is returning a function and the second arg to eval.

-- so function with 1 arg is reduced like this:
-- 1) \x.x
-- 2) (\x.x) 1
-- 3) [x:=1]
-- 4)     1

-- so function with 2 args is reduced like this:
-- 1) \xy.xy
-- 2) (\xy.xy) 1 2
-- 3) (\x(\y.xy)) 1 2   -- make currying explicit.
-- 4) [x := 1]
-- 5) (\y.1y) 2  returns a function that takes last argument.
-- 6) [y := 2]  bind y with 2 (the final arg)
-- 7) 1 2 is final reduction. and last head is dropped.
-- this is basically 2 nested identity functions

-- Lets do one that is more interesting:

-- 1) \xy.xy
-- 2) (\xy.xy)(\z.a) 1 -- apply 2 args to \xy.xy
-- 3) (\x(\y.xy))(\z.a) 1  -- make currying explicit.
-- 4) [x := (\z.a)]
-- 5) (\y.(\z.a)y) 1  x is bount to 1st arg(\z.a) head is dropped
-- 6) [y := 1]    -- y is bound to 2nd arg   next head is dropped
-- 7) (\z.a) 1 apply this one more time
-- 8) [z := 1  no z in body so nowhere to put 1  just drop head.
-- 9) final result is 'a'.

-- More common to apply abstract vaiables than concrete values like we did above with 1's and 2's.
-- like this:
-- (\xy.xxy)(\x.xy)(\x.xz)  but this becomes a tangle of mess of variables.

-- so:

-- 1) (\xyz.xz(yz))(\mn.m)(\p.p) --original
       -- make currying explicit

-- 2) (\x.\y.\z.xz(yz))(\m\n.m)(\p.p)
         --sub  x      (\m\n.m)

-- 3) (\y.\z(\m\n.m)z(yz))(\p.p)
              --sub   y   (\p.p)

-- 4) \z(\m\n.m)(z)((\p.p)z)
--     |-- outermost lambda z irreducible no more args

-- go inside terms one layer at a time until something can reduce
--    \z(\m\n.m)(z)((\p.p)z)
     --bind m-|  |--to arg z

-- 5) \z(\n.z)((\p.p)z
-- bind   n   ((\p.p)z)
-- 6)       z       .z
-- 7)      \z.z
-- 8)       z
       --not completly sure how last steps happen


--Intermission: Quivalence Exercises

--  1) \xy.xz
--  b) \mn.mz   -- Alpha equivalence

--  2) \xy.xxy
--  c) \a(\b.aab)

--  3) \xyz.zx
--  b) \tos.st


-- 1.7 Evaluation is simplification

--beta normal form == no further lambdas to apply to arguments. In programming this corresponds to a fully evaluated expression/ fully executed program. Evaluation is a form of simplification in Haskell code. Normal form == beta normal form.

-- 'Application' is what makes evaluation/simplification possible.

-- (10 + 2) * 100 / 2  reduces or evaluates to 600.
--     12   * 100 / 2
--           1200 / 2
--                600

-- the identity function: \x.x is fully reduced untill to apply arguments to it like :  (\x.x)z -- it then can be reduced/simplified to:
-- final result/evaluates to 'z' the identity of the argument 'z'.
-- its beta normal form would be 'z'.

-- 1.8 Combinators

-- a combinator is a lambda term with no free variables. they only serve to 'combine' the arguments they are given.

-- the following are combinators:
-- 1) \x.x
-- 2) \xy.x
-- 3) \xyz.xz(yz)

-- the following are not combinators / they have one or more 'free' variables:
-- 1) \y.x -- y is bound(it is in head) but x if 'free'
-- 2) \x.xz -- x is bound but z is 'free'

-- 1.9 Divergence

-- some lambda terms are not reducable into beta normal form. it is because they 'diverge'. 'divergence'  here means the reduction process never terminate or ends. normally lambda terms will 'converge' into a beta normal form. but some will 'diverge' or never finish evaluating to a value.

-- this lambda term is called omega and it 'diverges':
-- 1) (\x.xx)(\x.xx)  x in the first lambda's head becomes the second lambda.
-- 2) ([x := (\x.xx)]xx)
--    Using [var := expr] to denote what x is bound to.
-- 3) (\x.xx)(\x.xx)
--    substituting (\x.xx) for each occurence of x. we end up where we started and this keeps going and going and going.
--    so we say:  'omega diverges'
--    this matters in programming. understanding what will terminate means understanding what programs will do useful work and return the answer we want.

-- 1.10 Summary

-- * functional programming is based on expressions that include variable or constant values, expressions combined with other expressions, and functions.

-- * functions have a head and body that can be applied and reduced or evaluated to a resulting value.

-- * variables may be bound in a fuction declaration and they  will have the same value when ever is in the same scope.

-- * all functions in haskell take one argument and return one result.. and always the same output  with the same input

-- * functions are a mapping of a set of inputs to a set of outputs. given the same input will always produce the same output.

-- 1.11 chapter exercises

-- 1) \x.xxx   yes it is a combinator

-- 2) \xy.zx   no because it has 'free' variables

-- 3) \xyz.xy(zx) yes it is a combinator

-- 4) \xyz.xy(zxy)  yes it is a combinator

-- 5) \xy.xy(zxy)  no it has 'free' vaiables

--    yeah i got them right but im introuble below

--Normal form or diverge

-- 1) \x.xxx  does not diverge /only if it was applied to itself

-- 2) (\z.zz)(y.yy)  diverges  i'm pretty sure  yep it's omega again.

-- 3) (\x.xxx)z normal form is zzz

--Beta reduce the following

-- 1) (\abc.cba)zz(\wv.w)

--    (\a.\b.\c.cba)(z)z(\w.\v.w))   --explecit currying
--                |  |                 drop \a. head

--    (\b.\c.cbz)(z)(\w.\v.w)
--            |   |                    drop \b. head

--    (\c.czz)(\w.\v.w)
--        |   |-------|                drop \c. head

--    (\w.\v.w)(z)z
--           |  |                      drop \w. head

--    (\v.z)(z)
--        |  |                         drop \v. head

--        z          evaluates to z

-- did not do 2...6   todo

-- 7)
--    a) (\xyz.xz(yz))(\x.z)(\x.a)
--       add emplied lambda to introduce each arg.

--    b) (\x.\y.\z.xz(yz))(\x.z)(\x.a)
--         *       |      |----|       drop \x. in head

--    c)     (\y.\z1.(\x.z)z1(yz1))(\x.a)
--             *              |    |----|  drop \y. in head

--     d)         (\z1.(\x.z)(z1)((\x.a)z1))
--                       *    |   |----|    drop \x. in head

--     e)         (\z1.z((\x.a)(z1)))
--                         *    **       drop \x. z1

--     f)           (\z1.za)       ??????????? how ????????????

-- this is not clear to me yet

-- 1.13 Definitions

-- 1) the 'lambda' in lambda calculus in the greek letter \ is used to introduce, or abstract, arguments for binding in an expression.

-- 2) a lambda 'abstraction' is an anonymous function or lambda term.
--    (\x.x + 1)

-- 3) 'application' is how one evaluates lambdas, applying lambda to arguments until you run out of arguments to apply lambdas to.

-- 4) 'lambda calculus' is a formal system for expressing programs.

-- 5) 'normal order' is a common evaluation strategy in lambda calculi. by applying or beta reducing the leftmost outermost lambdas first.

-- 'normal orderl is NOT how haskell code is evaluated. haskell code is evaluated in 'call-by-need' instead.

