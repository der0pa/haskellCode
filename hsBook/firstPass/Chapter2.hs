module Chapter2 where

--notes from chapter two of the haskell book.
-- Hello, Haskell!

-- i have emacs haskell-mode set up pretty well.
-- i can C-c, C-l to load current buffer into repl

sayHello :: String -> IO()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

{-| C-c, C-l produces:

λ>
λ> sayHello "Haskell"
Hello, Haskell!
λ>

-}

-- 2.3 Understanding expressions

-- expressions are the building blocks of our programs.
-- programs themselves are one big expression made of smaller expressions.

{-|

λ> 1
1
λ> 1 + 1
2
λ> "Icarus"
"Icarus"
λ>

expressions can be nested:

λ>
λ> (1 + 2) * 3
9
λ> ((1 + 2) * 3) + 100
109
λ>

Normal form  -we say that expression are in 'normal form' when no
more evaluations steps can be taken

2.4 Functions

expressions are the basic unit of a haskell program, and functions
are a specific type of expression.

like lambda calculus haskell functions take one argument and return
one result.

functions are how we factor out the pattern into something we can
reuse with different inputs.

Defining functions:
-}

triple x  =  x * 3
--[1]-[2][3]-[ 4 ]

--1) function name is always lower case
--2) x is the parameter ... same a head in lambda calculus
--3) = is used to define or declare values .. not a test of equality
--4) the body of the function
{-|
λ> triple 2
6
λ>

2.5 Evaluation

the process of reducing the terms of expression until the it reaches
its simplest form. called irreducible 'lazy evaluation' nonstrict evaluation.

Values are irreducible
Values are expressions that cnnot be reduced further.
Values are a terminal point of reduction.

λ> 1
1
λ> "Icarus"
"Icarus"
λ>

the following can be reduced-evaluated to a value:

λ> 1 + 1
2
λ> 2 * 3 + 1
7
λ>

triple 2
--[triple x = x * 3; x:= 2]
2 * 3
6

Exercises: Comprehension Check

half x = x / 2       in source file

let half x = x / 2   in repl

square x = x * x     in source file

let square x = x * x   in repl

2) write a function that will take one argument for all the following:

3.14 * (5 * 5)
3.14 * (10 * 10
3.14 * (2 * 2)
-}

myPi x =
  3.14 * (x * x)

{-|
λ> myPi 5
78.5
λ> myPi 10
314.0
λ> myPi 2
12.56
λ>

rewrite myPi to use Prelude value called 'pi' not 3.14

-}

myPiTwo x =
  pi * (x * x)

{-|
λ> myPiTwo 5
78.53981633974483
λ> myPiTwo 10
314.1592653589793
λ> myPiTwo 2
12.566370614359172
λ>

2.6 Infix operators

haskell functions are prefix by default. but also has infix syntax.

many basic math operators are infix (+) (-) (*) (/) and most are left associative

this
2 * 3 * 4
is evaluated a if it was
(2 * 3) * 4
because of 'left-associativity'

exponentiation is (^) right associative
so:
λ> :info ^
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in ‘GHC.Real’
infixr 8 ^
λ> 2 ^ 3 ^ 4
2417851639229258349412352
λ> 2 ^ (3 ^ 4)
2417851639229258349412352
λ> (2 ^ 3) ^ 4
4096
λ>




--2.7 Declaring values

x = 10 * 5 + y
myResult = x * 5
y = 10

-}
-- formatting / whitespace sensitive 4- paces for indentation

{-
Operator Name       Purpose/application
+        plus       addition
-        minus      subtraction
*        asterisk   multiplication
/        slash      fractional division
div      divide     integral division, round down
mod      modulo     like ‘rem’, but after modular division
quot     quotient   integral division, round towards zero
rem      remainder  remainder after division


Laws for quotients and remainders

λ> (quot x y)*y + (rem x y)
10
λ> (quot 10 (-4))*(-4) + (rem 10 (-4))
10
λ> quot 10 (-4) == (-2)
True
λ> rem 10 (-4) == 2
True
λ> (-2)*(-4) + (2) == 10
True
λ> 10 == x == yeppers

Now for 'div' and 'mod'

λ> (div 10 (-4))*(-4) + (mod 10 (-4))
10
λ> div 10 (-4) == (-3)
True
λ> mod 10 (-4) == -2
True
λ> (-3)*(-4) + (-2) == 10
True
λ> 10 == x
True
λ>

Using 'mod'

Modular arithmetic is a system for integers that 'wrap arount' on reaching a certain value called modulus.

if it is 8 oclock and you want to now the time 8 hours from now.
you write the expression:

λ> mod (8 + 8) 12
4

or if it is 10 oclock and you need to know what time it was 18 hours priviously you would write an expression like:

λ> mod (10 - 18) 12
4

you could find out what day of the week it was 10 days from sunday wouldbe wednesday.

sunday = 0 , monday = 1, tuesday = 2, wednesday = 3,
friday = 4, saturday = 5.

λ> mod (0 + 10) 7
3

or 23 days before last tuesday would be a sunday

λ> mod (2 - 23) 7
0

Syntax is the grammar and structure
of the text we use to express programs, and syntactic
sugar is a means for us to make that text easier to read and
write.

Syntactic sugar can make the typing or reading of code
nicer but changes nothing about the semantics, or meaning, of
programs and doesn’t change how we solve problems in code.

(-) is the subtraction function

9 (-) 2 = 7

λ> (-) 9 2
7

λ> 9 + (-2)
7

λ> 9 + -2
<interactive>:113:1-6: error:
    Precedence parsing error
        cannot mix ‘+’ [infixl 6] and prefix `-' [infixl 6] in the same \
infix expression

2.9 Parenthesization

you can use $ to replace parens

λ> (2 ^) (2 + 2)
16

λ> (2 ^)  2 + 2
6

λ> (2 ^)  $ 2 + 2
16

you can also put infix functions in parens and use them prefix style.

λ> 1 + 2
3

λ> (+) 1 2
3

λ> (+1) 2
3

2.10 Let and where
-}

printInc n = print plusTwoA
  where plusTwoA = n + 2

printInc2 m =
  let plusTwoB = m + 2
  in print plusTwoB

{-
λ> printInc 10
12

λ> printInc2 40
42

λ> let x = 5 in x
5

λ> let x = 5 in x * x
25

λ> let x = 5; y = 6 in x * y
30

λ> let x = 3; y = 1000 in x +3
6


multi = x * y
  where x = 5
        y = 6


-- let x = 3; y = 1000 in x * 3 + y
-- rewrite a with 'where clauses:

someMath = x * 3 + y
  where y = 1000
        x = 3


λ> someMath
1009



-- let y = 10; x = 10 * 5 + y in x * 5
-- rewrite with 'where'

someMath2 = x * 5
  where y = 10
        x = 10 * 5 + y


λ> someMath2
300

λ> x
60

λ> y
10

-}

-- let x = 7 ; y = negate x; z = y * 10 in z / x + y
-- rewrite using where clauses:

someMath3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

{-

λ> someMath3
-17.0

λ> x
<interactive>:21:1: error: Variable not in scope: x

-}

waxOn = x * 5
  where z = 7
        x = y ^ 2
        y = z + 8

waxOff x = triple x

{-|

2.12 Definitions

1.  'parameter' == represent a value to will be passed to a function.
    'argument' == an input value the function is applied to.

    a functions parameter is bound to the value of an argument when the
    function is applied to that argument.... ????
2.  an 'expression' is a combination symbols that conforms to syntactic     rules and can be evaluated to some result.

3.  'redex' is a reducible exression

4.  'value' is a expression that cannot be reduce or evaluated any further.

5.   'function' is a mathematical object whose capabilities blah blah blah
-}


multi =
  x * y
  where x = 5
        y = 6
