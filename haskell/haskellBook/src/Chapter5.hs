{-# LANGUAGE NoMonomorphismRestriction #-}

module Chapter5 where

{-| 5.1 Types

Functions: match functions to their type annotations.

a) not
c) not :: Bool -> Bool

b) length
d) length :: [a] -> Int
   length :: Foldable t => t a -> Int

c) concat
b) concat :: [[a]] -> a
   concat :: Foldable t => t [a] -> [a]

d) head
a) head :: [a] -> a

e) (>)
e) (>) :: Ord a => a-> a -> Bool


5.4 Curring

λ> :t (+)
(+) :: Num a => a -> a -> a
--     |  1   |

(+) :: Num a => a -> a -> a
--              |   2   |

(+) :: Num a => a -> a -> a
--                       [3]

1) typeclass constraint that 'a' must have instance of Num

2) successive function applications, each taking one argument and returning one result. this is called curring.

3) this is final result/value type for the function. the 'a' means it is the same type as privious args.

-}

--Parial application ...


-- we use double colon to assign a type
-- making the type concrete will eleminate
-- the typeclass constraint

addStuffAndFive :: Integer -> Integer -> Integer
-- with explicit parenthesization
-- addStuff :: Integer -> (Intiger -> Integer)
addStuffAndFive a b =
  a + b + 5

addTen :: Integer -> Integer
addTen =
  addStuffAndFive 5

fifteen =
  addTen 5

{-

λ> fifteen
15

λ> addTen 15
25

λ> addStuffAndFive 5 5
15

Here the function fifteen == addStuffAndFive 5 5
because the function addTen == addStuffAndFive 5


applying only some of a function’s arguments
is called partial application, which lets us reuse addStuffAndFive
and create a new function (addTen) from it with one of the arguments
applied.


-}

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y =
  x - y - 10

subtractOne :: Integer -> Integer
subtractOne =
  subtractStuff 1

result =
  subtractOne 11

{-
λ> result
-20

λ> result == 1 - 11 - 10
True

-}

-- why did we get 11 as the result?
-- because the order we applied the arguments.
--result == 1 - 11 - 10

{-|

λ> 1 - 11 - 10
-20

λ> 2 - 11 - 10
-19

Some older FP langs default to using product type (tuples) to express multiple args.


-}

nonsense :: Bool -> Integer
nonsense True  = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b =
  i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) =
  i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested =
  \i -> \b -> i + (nonsense b)

x = 5
y = (2^)
z = (^2)

{-|
λ> x
5
λ> z x
25
λ> z
z :: Num a => a -> a
λ> x
5

λ> y
y :: Integer -> Integer

λ> z
z :: Integer -> Integer

λ> x
5

λ> z x
25

λ> y x
32

λ> 2^ 5
32

λ> 2*2*2*2*2
32

λ> y x
32

λ> z x
25

-}

celebrate =
  (++ " woot!")


{-|

λ> celebrate "naptime"
"naptime woot!"

λ> :t celebrate
celebrate :: [Char] -> [Char]

λ> celebrate "dogs"
"dogs woot!"

Typeclass restraints:

(Num a, Num b) => a -> b -> b
or
(Ord a, Num a) => a -> a -> Ordering

everything to the left of => does not show up in term level

the tuple of constraints represents a product or conjunction of constraints

the second example constraint are that 'a' variable must be a type that implements BOTH Ord and Num

* uncurried function: one function, many args
* curried function:  many fuctions, one arg for each function.

Exercises: Type Matching

1. Functions:

  a) not
     not :: Bool -> Bool

  b) length
     length :: [a] -> Int
     length :: Foldable t => t a -> Int

  c) concat
     concat :: [[a]] -> [a]
     concat :: Foldable t => t [a] -> [a]

  d) head
     head :: [a] -> a

  e) (<)
     (<) :: Ord a => a -> a -> Bool



λ> :i (->)
data (->) t1 t2 	-- Defined in ‘ghc-prim-0.5.0.0:GHC.Prim’
infixr 0 `(->)`


f :: a -> a -> a

-- because (->)  is right associtive
-- associates to

f :: a -> (a -> a)

the parenthese is not for control precedence or order of eval.
they only serve to group the parameters in (argument and result)

remember when we have a lambda expression that appears to have two parameters, it is really just nested lambdas. applying the first function returns a (higher-order-function) that can then take the final args and return a value/result.

application is evaluation; evaluation of anything happens because of function-application and it is left associative. so left most/outter most args are evaluated first. if anything is evaluated (haskell is lazy)


Currying and uncurrying existing functionssss
-}

--below is different in current GHCi
curry' f a b = f (a,b)
curry' :: ((t1, t) -> t2) -> t1 -> t -> t2
-- 3 args |------f------|     a    b    |-result type == t2

{-|

λ> :t fst
fst :: (a, b) -> a

λ> :t curry' fst
curry' fst :: t2 -> b -> t2

λ> curry' fst 1 2
1

λ> fst (1,2)
1


Sectioning
  the term sectioning specifically refers to partial applicatin of infix operators. with syntax that allows you to choose whether the partial application is to the first or second argument

λ> let x = 5
λ> let y = ( 2 ^ )
λ> let z = ( ^ 2 )

λ> y x
32

λ> z x
25


Exercises : Type Arguments

1) a) Char -> Char -> Char

λ> let f :: a -> a -> a -> a; f = undefined

λ> f
f :: a -> a -> a -> a

λ> let x :: Char; x = undefined

λ> :t f x
f x :: Char -> Char -> Char

2) d) Char

λ> let g :: a -> b -> c -> b ; g = undefined

λ> :t g 0 'c' "woot"
g 0 'c' "woot" :: Char

3) d) Num b => b

λ> let h :: (Num a, Num b) => a -> b -> b; h = undefined

λ> :t h 1.0 2
h 1.0 2 :: Num b => b

compiler must assume that a and b could be different

4) c) Double

λ> :t h 1 (5.5 :: Double)
h 1 (5.5 :: Double) :: Double


5) a) [Char]

λ> jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined

λ> :t jackal "keyboard" "has the word jackal in it"
jackal "keyboard" "has the word jackal in it" :: [Char]

6) e) Eq b => b -> [Char]

λ> :t jackal "keyboard"
jackal "keyboard" :: Eq b => b -> [Char]


7) d) (Num a, Ord a) => a

λ> let kessel :: (Ord a, Num b) => a -> b -> a ; kessel = undefined

λ> :t kessel 1 2
kessel 1 2 :: (Num a, Ord a) => a

8) a) (Num a, Ord a) => a

λ> let kessel :: (Ord a, Num b) => a -> b -> a ; kessel = undefined

λ> :t kessel 1 (2 :: Integer)
kessel 1 (2 :: Integer) :: (Num a, Ord a) => a

9) c) Integer

λ> let kessel :: (Ord a, Num b) => a -> b -> a ; kessel = undefined

λ> :t kessel (1 :: Integer) 2
kessel (1 :: Integer) 2 :: Integer


5.5 Polymorphism

polymorphic == 'made of many forms'

haskell polymorphism divides into two categories:
 1. parametric polymorphism (broader than ad-hoc) type-vars or parameters- unconstrained by typeclass.
 2. constrained polymorphism (ad-hoc) implemented with typeclasses

'a' is parametrically polymorphic in the identity function

id :: a -> a

it allows the id function to work will any type of data

a function is poymorphic when its type signature has variables that can represent more than one type.

Excercises: Parametricty

1) type a -> a which is the type for 'id' is unable to return anything other than the type of one parameter.

2) type a -> a -> a

   f :: a -> a -> a
   f a b = a
   f b a = b

3) type a -> a -> b
   f _b = b

not sure about these.

Polymorphic constants

λ> (-10) + 6.3
-3.7

λ> :t (-10) + 6.3
(-10) + 6.3 :: Fractional a => a

λ> :t (-10)
(-10) :: Num a => a

λ> :t 6.3
6.3 :: Fractional t => t

-}
--Type inference

ourId x =
  x
{-|

λ> :t ourId
ourId :: t -> t

λ> myGreet "Hello"
"Hello Julie"

λ> :t myGreet
myGreet :: [Char] -> [Char]

-}

myGreet x =
  x ++ " Julie"


--f :: Num a => a -> a  -> a
f x y =
  x + y + 3

myConcat x =
  x ++ " yo"

myMult x =
  (x / 3) * 5

myTake x =
  take x "hey you"

-- 5.8 Chapter Exercises

-- a value of type [a] is
--   c) a list whose elements are all some type a

-- a function of type [[a]] -> [a] could
--   a) take a list of strings as an argument

-- a function of type [a] -> Int -> a
--   b) returns one element of type a from a list

-- a function of type (a,b) -> a
--   c) takes a tuple argument and returns the first 'fst' value

-- Determine the type



example = 1

bigNum = (^) 5 $ 10

wahoo = bigNum ^ 10



