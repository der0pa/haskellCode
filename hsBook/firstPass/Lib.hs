module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunctionHere"
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

{-| castillo dds 706-886-4256 next monday 3:30
-}
hypotenuse a b = sqrt (a ^ 2 + b ^ 2)
