-- Practice.hs
module Practice where

multi =
  x * y
    where x = 5
          y = 6
          
--rewrite let functions into where's

f =
  x * 3 + y
    where x = 3
          y = 1000

f2 =
  x * 5
    where y = 10
          x = 10 * 5 + y
f3 =
  z / x + y
    where x = 7
          y = negate x
          z = y * 10