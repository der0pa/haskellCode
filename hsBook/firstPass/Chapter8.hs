module Chapter8  where

import Data.List ( intercalate)



{-| recursion in a 'lambda calculus' is done using the 'y combinator-
    or fixed-point combinator-}



factorial :: Integer -> Integer
factorial 0 = 1  --stopping point or 'base case'
factorial n = n * factorial (n - 1)    

--lets look at some similar patterns
       
inc :: Num a => a -> a
inc = (+1)

three :: Integer
three = inc . inc . inc $ 0

--general function that can apply 'inc' an certain amount of times.

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n           --base case here
incTimes times n = inc (incTimes (times - 1)n)         

--abstract the recurion out of incTimes
           
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
--applyTimes n f b = f (applyTimes (n-1) f b)                          
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times  = applyTimes times inc 

--Intermission: Exercise

{-| Write out the evaluation of the following
    
    applyTimes 5 (+1) 5
    10

       applyTimes 5 (+1) 5  = f (applyTimes (n-1)  f    b
                  n  f   b   (+1)           (5-1) (+1)  5
       = (+1) . applyTimes (5-1) (+1) $  5o
       = (+1) . (+1) . applyTimes (4-1) (+1) $ 5                              = (+1) . (+1) . (+1) . applyTimes (3-1) (+1) $ 5
       = (+1) . (+1) . (+1) . (+1) . applyTimes (2-1) (+1) $ 5
 base  = (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes (1-1) (+1) $ 5
       = (+1) . (+1) . (+1) . (+1) . (+1) . (+1) $ 5
       = (+1) . (+1) . (+1) . (+1) . (+1) $ 5
       = (+1) . (+1) . (+1) . (+1) $ 6
       = (+1) . (+1) . (+1) $ 7
       = (+1) . (+1) $ 8
       = (+1) $ 9
       = (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) 5
       = (+1) . (+1) . (+1) . (+1) . (+1) 5
       = (+1) . (+1) . (+1) . (+1) 6
       = (+1) . (+1) . (+1) 7
       = (+1) . (+1) 8
       = (+1) 9
          10

λ> applyTimes 5 inc 5  --apply inc 5 times starting at 5
   10

 -}

--8.4 Fibonacci numbers
      
-- a sequence of numbers in which each number is the sum of the
   --previous two:  1,1,2,3,5,8,13,21,34...
   
fibonacci :: Integral a => a -> a 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)   
          
{-| fib 6 = fib 5 + fib 4
    fib 5 = fib 4 + fib 3
    fib 4 = fib 3 + fib 2
    fib 3 = fib 2 + fib 1
    fib 2 = fib 1 + fib 0 --base case
    fib 0 + 0
    fib 1 + 1
    fib 2 + (1 + 0=) 1
    fib 3 + (1 + 1=) 2
    fib 4 + (1 + 2=) 3
    fib 5 = (2 + 3=) 5
    fib 6 = (3 + 5=) 8
    
    -}          

    --now lets try to do division with only subraction


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom  = go num denom 0
  where go n d count | n < d = (count, n)
                     | otherwise = go (n - d) d (count + 1)

{-| 10 div 2 == 10 - 2,8 count = 1 
                   - 2,6 count = 2
                   - 2,4 count = 3
                   - 2,4 count = 4
                   - 2,0 count = 5
divideBy 10 2 =
         go 10 2 0  -- n d c
         


                      -}


func :: [a] -> [a] -> [a]
func x y = x ++ y         

mc91 :: (Num t, Ord t) => t -> t
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))
                     
  
-- input an Int and return its name in a String
digitToWord :: Int -> String 
digitToWord n = 
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> "undefined"


digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits(n `div` 10) ++ [n `mod` 10]

          -- wordNumber 123  -> "one-two-three"
wordNumber :: Int ->String
wordNumber n = intercalate "-" (map digitToWord (digits n) )



