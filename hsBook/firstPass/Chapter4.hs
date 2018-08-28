module Chapter4 where

{-|

Basic datatypes

... install
some means of
understanding in the very
programs themselves.
Robin Milner

Expression reduce to values.
Every value has a type.

'Data declarations' are how datatypes are difined.
'Type constructor' is the name of the type and is Capitalized.
'Data constructors' are the values that show up in your code.

-- the definition of Bool in haskell

data Bool = False | True   == data declaration
      [1]    [2] [3] [4]

1. Type constructor for datatype Bool
2. Data constructor for value False
3. Pipe | indicate a 'sum type' or logical disjunction: "or"
   a Bool value is True or False
4. Data constructor for value True


λ> :i not
not :: Bool -> Bool 	-- Defined in ‘ghc-prim-0.5.0.0:GHC.Classes’

λ> not True
False

λ> not False
True


-}

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood mood =
  case mood of
    Blah -> Woot
    Woot -> Blah

--Integral numbers   types: Int and Integer

-- Interal numbers are whole number with no fracional component

-- Integer should be used unless Int limitations are understood as Int has slightly better perfomance.

-- Why even have Int? Int  is an artifact of what hardware has supported natively over the years.

{-
λ> import GHC.Int

λ> 127 :: Int8
127

λ> 128 :: Int8

<interactive>:30:1-3: warning: [-Woverflowed-literals]
    Literal 128 is out of the Int8 range -128..127
    If you are trying to write a large negative literal, use NegativeLiterals
-128

λ> import GHC.Int

λ> minBound :: Int8
-128

λ> maxBound :: Int8
127

λ> minBound :: Int16
-32768

λ> minBound :: Int32
-2147483648

λ> minBound :: Int64
-9223372036854775808

λ> maxBound :: Int8
127

λ> maxBound :: Int16
32767

λ> maxBound :: Int32
2147483647

λ> maxBound :: Int64
9223372036854775807

-}

--Fractional numbers

-- 1) Float -- used is graphics
-- 2) Double  -- don't explicitly use Double
-- 3) Rational  --arbitrary-precision
-- 4) Scientific  --arbitrary-precision

--data Bool = False | True

-- datatype Bool is represented the the values 'True' or 'False'

-- data constructors are Capitalized

-- && == conjunction so is 'and'
-- || == disjunction so is 'or'


-- Structure of 'if/then/else' expression
 -- a way to choose between two values

-- if CONDITION
-- then EXPRESSION\_A
-- else EXPRESSION\_B

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhhhh."
  where cool v = v == "downright frosty yo"

-- 4.7 Tuples

-- two-tuple or pair == (x,y) x and y 'can' be different types
-- three-tuple == (x,y,z)  etc....

{-
type level and terminal level use (,) constructor

λ> :i (,)
data (,) a b = (,) a b 	-- Defined in ‘ghc-prim-0.5.0.0:GHC.Tuple’

tuple is a 'product' type that is conjunction: you must supply both args

λ> (,) 8 10
(8,10)

λ> (,) 8 "Julie"
(8,"Julie")

λ> :i fst
fst :: (a, b) -> a 	-- Defined in ‘Data.Tuple’

λ> :i snd
snd :: (a, b) -> b 	-- Defined in ‘Data.Tuple’

λ> let myTup = (1 :: Integer, "blah")
λ> myTup
(1,"blah")

λ> fst myTup
1

λ> snd myTup
"blah"

λ> import Data.Tuple
λ> swap myTup
("blah",1)

λ> myTup
(1,"blah")


4.9 Lists

more on lists in the 'List' chapter

lists literal are created by

1:2:3:[]
[1,2,3]

4.9 Chapter Exercises
-}

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

{-
1. length is a function that takes a [list] and returns how many items are in it -> Int

2. what are results of the following?

λ> length  [1,2,3,4,5]
5

λ> length [(1,2),(2,3), (3,4)]
3

λ> length awesome
3

λ> length (concat allAwesome)
5

3.

λ> 6 / length [1,2,3]

<interactive>:581:1-18: error:
    • No instance for (Fractional Int) arising from a use of ‘/’
    • In the expression: 6 / length [1, 2, 3]
      In an equation for ‘it’: it = 6 / length [1, 2, 3]

λ> 6 `div` (length[1,2,3])
2

5. what is the type of 2 + 3 == 5
                       Int -> Int -> Bool
6. type and expected result of:

λ> let x = 5

λ> x + 3 == 5   --False
False

λ> length [1, 'a', 3, 'b' ]  --error not all same type in list

λ> length allAwesome == 2 --True
True

λ> length allAwesome + length awesome  -- 5
5

λ> (8 == 8) && ('b' < 'a')  -- False
False

λ> (8 == 8) && 9   --False WRONG -- Error cuz 9 is not a bool expression

-}

-- 8. write a function to determine a String is a 'palendrome'

isPalendrome :: (Eq a) => [a] -> Bool
isPalendrome str =
  str == reverse str

alsoIsPalendrome str =
  stripped == reverse stripped
  where stripped = filter (/= ' ') str



{-
λ> isPalendrome "radar"
True

λ> isPalendrome "taco cat"
False

λ> alsoIsPalendrome "taco cat"
True


λ> isPalendrome "tacocat"
True

-}

myAbs :: Integer -> Integer
myAbs x =
  if x >= 0  then x  else (-x)

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y =
  ((snd x, snd y),(fst x,fst y))

{-

λ> :i f
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  	-- Defined at /home/ubuntu/Code/haskell/haskellBook/src/Chapter4.hs:281:1

λ> f ("jim", 33) ("sue", 22)
((33,22),("jim","sue"))

a function that adds 1 to length of a string

incorrect version:

x = (+)   -- x == add function

F xs = w 'x' 1
  where w = length xs
-}
-- corrected version

myAdd = (+)

xsLength xs =  --function name is always lowerCase
  w `myAdd` 1   -- needed to use `` (backTic) not ''(singleQuote)
  where w = length xs

{-

λ> xsLength "my string == xs"
16

λ> length "my string == xs"
15


incorrect version of identity function:

\X = x
\x = x  -- lowerCase x not X

incorrect version:

\ x : xs -> x

 as a lambda

-}

annon = \(x:xs)-> x

-- as a named function
returnHeadOfList :: [a] -> a
returnHeadOfList  x =
  head x

{- incorrect version:

f(a b)= A

corrected version :

λ> f(a,b) = a

λ> f(1,2)
1
:t show
c) show :: Show a => a -> String

:t (==)
c) (==) :: Eq a => a -> a -> Bool

:t fst
a) fst :: (a,b) -> a

:t (+)
d) (+) :: Num a => a -> a-> a

4.10 Definitions

1.) tuple == ordered grouping of values  (a,b) or unit type ()

2.) typeclass == a set of operations defined with respect to polymorphic type.

3.) Data constructor == provide a means of creating values of a given type.
-}

type Name = String

data Pet
  = Cat
  | Dog Name

{-

λ> :t Cat
Cat :: Pet

λ> :t Dog
Dog :: Name -> Pet

4.) Type constructors are not values and can only be used in type signatures/annotations. Above 'Pet' is the type constructor. on left side of '=' in a data declaration.

5.) Data declarations define new datatypes in Haskell. Data declarations always create a new type constructor. many may or may not create new 'data' constuctors. Data declarations are how we refer to the entire definition. begins with the 'data' keyword.

6.) A 'type alias' is a way to refer to a type consructor or constant with an alias or alternate name.

like:

type Name = String

--not a new Type/data declaration just an alias

String is actually an alias for [Char] ie: list of chars

7.) Arity is the number of arguments a function accepts. all functions in Haskell take just one argument due to currying, function with multiple arguments return a function to apply to sencond arg .. etc.etc.

8.) Polymorphism in Haskell means writing code in terms of values which may be one of several or any, type. parametric or contrained are two catagories of polymorphism. the identy function is an example of a parametric function

id :: a -> a
id x = x

the identity function works with values of any type.

while the function isEqual is 'constrained' or bound to types which are of the type class Eq.
-}
isEqual :: Eq a => a -> a -> Bool
isEqual x y =
  x == y
{-

λ> isEqual "dog" "dog"
True

λ> isEqual 42 42
True

λ> isEqual "dog" "cat"
False

-}


main :: IO ()
main = print "hello"
