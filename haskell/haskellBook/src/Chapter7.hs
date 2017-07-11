module Chapter7 where

{- | structured haskell mode
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")

-- | Skeletons module if case do let
-- | Delimiters "" () {} {--} gliding
-- | Auto-reindenting Cntr < or >
-- | Yanking and killing  CM-k (k-node) C-k shm kill  C-y shm kill
-- | Newline-indent C-j newline and indent
   
   -}

--comment from oldBox

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum
  
          
          



bindExp :: Integer -> String
bindExp x = let y = 5
            in "the integer was: " ++ show x
            ++ " and y was: " ++ show y
{-
 λ> bindExp 20
 "the integer was: 20 and y was: 5"
-}

{-
addOne :: Integer -> Integer
addOne x = x + 1
addOne1 = \x -> x + 1
-}
  





triple :: Integer -> Integer
triple x = x * 3

-- Exercises: Grab Bag

mTha x y z = x * y * z

mThb x y = \z -> x * y * z

mThc x = \y -> \z -> x * y * z

mThd = \x -> \y -> \z -> x * y * z

-- all the above are ==

addOneIfOdd n =
  case odd n of
    True  -> f n
    False -> n
      -- where f n = n + 1
  where
    f = \n -> n + 1
--   or even 
--   where \f -> \n -> n + 1

--Rewrite using anonymous lambda syntax:
-- addFive x y = (if x > y then y else x) + 5


isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False             



data WherePenguinsLive =
     Galapagos
   | Antarctica
   | Australia
   | SouthAfrica
   | SouthAmerica
   deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

{- 
   
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False
   
   -}
--redundant

--using pattern matching
        
gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

-- in this final function, the || operator
-- is an `or` function, which will return True
-- if either value is True
   
antarticOrGalapagos :: Penguin -> Bool
antarticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)


--pattern matching on tuples:
          
fa :: (a,b) -> (c,d) -> ((b,d), (a,c))
fa (a,b) (c,d) = ((b,d), (a,c))
  
  
fb :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fb (a, _, c)  (d, _, f) = ((a, d), (c, f)) 
   

funcZ x = 
    case x + 1 == 1 of
      True -> "Awsome" 
      False -> "wut"


pal :: String -> String
pal xs = 
    case y of
      True -> "yea"
      False -> "na"
  where y = xs == reverse xs 

greetIfCool :: String -> IO ()
greetIfCool coolness = 
            case cool of
              True -> putStrLn "eyyy. What's shakein'?"
              False -> putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"

-- the following should return x when x is greater than y.
   
functionC x y =    
    if (x > y)
       then x
       else y

--rewrite to a case statement
          
functionD x y =
    case (x > y) of
      True -> x
      False -> y
        
ifEvenAdd2 n =
    if even n
       then (n + 2)
       else n
            
-- rewrite as a case statement
   
ifEvenAdd2b :: Integer -> Integer   
ifEvenAdd2b n = 
    case even n of
      True -> n + 2
      False -> n
  


nums x = 
    case compare x 0 of
     LT -> -1
     GT -> 1
     EQ -> 0

--7.6 Higher-order functions
      
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \x y -> f y x
                     
returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d
--same as:
returnLast' :: a -> (b -> (c -> (d -> d)))       
returnLast' _ _ _ d = d

{-

λ> returnLast' [1] [2] [3] [4]
[4]

λ> returnLast [1] [2] [3] [4]
[4]
        

returnBroke :: (((a -> b) -> c ) -> d ) -> d
returnBroke _ _ _ d = d               

src/Chapter7.hs:218:1-23: error: …
    • Couldn't match expected type ‘d’
                  with actual type ‘t2 -> t1 -> t0 -> t0’
      ‘d’ is a rigid type variable bound by
        the type signature for:
          returnBroke :: forall a b c d. (((a -> b) -> c) -> d) -> d
        at /home/ubuntu/someCode/haskell/haskellBook/src/Chapter7.hs:217:16
    • The equation(s) for ‘returnBroke’ have four arguments,
      but its type ‘(((a -> b) -> c) -> d) -> d’ has only one
    • Relevant bindings include
        returnBroke :: (((a -> b) -> c) -> d) -> d
          (bound at /home/ubuntu/someCode/haskell/haskellBook/src/Chapter7.hs:218:1)
Compilation failed.
λ> 

The type signature of returnBroke specifies a single function as
the sole argument to returnBroke.
   
   -}
   
returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)
 
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) 
                -> Employee 
                -> Employee 
                -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
--  [        1          ]
    EQ -> putStrLn "Neither employee is the boss"
--  [                  2                        ]
    LT -> (flip reportBoss)e e'
--  [           3             ]

-- 1) e > e' return reportBoss e e'
-- 2) e == e' return String     
-- 3) e < e' return (flip reportBoss) e e'

-- let us now show the real boss here
codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'


dodgy x y =
      x + y * 10

oneIsOne = dodgy 1
oneIsTwo = (flip dodgy)2


{-
λ> dodgy 1 0
1
λ> 0 * 10 + 1
1
λ> 1 + 0 * 10
1
λ> dodgy 1 1
11
λ> dodgy 2 2
22
λ> dodgy 1 2
21
λ> dodgy 2 1
12
λ> oneIsOne 1
11
λ> oneIsTwo 2
22
λ> oneIsOne 3
31
-}

{-x = 0
  
if (x + 1 == 1) 
   then "Awesome"
   else "wut"-}

myAbs :: Integer -> Integer
myAbs x = if x < 0
             then (-x)
             else x

      
      -- rewrite using guard blocks
      
myAbsGuard :: (Ord t, Num t) => t -> t
myAbsGuard x
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too hight"
  | otherwise = "just right"  

-- c is the hypotenuse of the triangle . Google it.

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "Right ON the money"
  | otherwise = "not really right"

dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'  
  where y = x / 100

palx xs
  | xs == reverse xs = True
  | otherwise        = False

-- what does the following return
   
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
    
-- if x is > 0 then 1
-- if x is == 0 then 0
-- if x is < 0 the -1
   
--(.) :: (b -> c) -> (a -> b) -> a -> c
--          [1]         [2]     [3]  [4]
          
          {-[1] function from b to c
            [2] function from a to c
            [3] value of type a /same a 1st arg of [2]
            [4] value of type c /same as type a result of [1]
          -}
-- then with the addition of one set of parens

--(.) :: (b -> c) -> (a -> b) -> (a -> c) 
--          [1]         [2]        [3]  

          {-The result of [2] is the arg of [1] so
            is this get us from an a arg to a c result 
          -}

-- composed functions /how to read and work with them

--(.) :: (b -> c) -> (a -> b) -> a -> c
--(f . g) x = f (g x)
                 
--(.) :: (b -> c) -> (a -> b) -> a -> c

f' :: Int -> [Int] -> Int
f'  = foldr (+) 

-- learn you a point-free!

add :: Int -> Int -> Int
add x y = x + y
          
addPF :: Int -> Int -> Int
addPF = (+)
  
addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)              

--(.) :: (b -> c) -> (a -> b) -> a -> c

--putStrLn : String -> IO ()
--              b        c                 (b -> c) 
--show . show :: Show a => a -> String                
--                      a         b        (a -> b)
--putStrLn . show :: Show a => a -> IO ()          
--                          a         c    (a -> c)       
                            
print :: Show a => a -> IO ()
print  = putStrLn . show 

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a 
doubleUs x y = doubleMe x + doubleMe y

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]
   
length' :: Num t => [t1] -> t          
length' [] = 0
length' (_:xs) = 1 + length' xs

lengthOfHam :: Integer
lengthOfHam = length' "ham"

{-| length' first checks for [] ... it is not empty 
    so it matches on line 2  and that evals to:
    1 + length' "am"         again checks for []  /not
    1 + (1 + length' "m")      again check for []   /not
    1 + (1 + (1 + length' []) )      now is is empty /yes
    1 + (1 + (1 + 0))
evaluated to: 3
-}

lengthOfHamburger :: Integer
lengthOfHamburger = length' "hamburger"
