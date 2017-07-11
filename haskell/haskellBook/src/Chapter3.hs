module Chapter3 where

{- Strings         programming is a play on words. Alan Perlis

3.1 Printing strings

Strings are a List of [Char]

λ> :t 'a'
'a' :: Char

λ> :t "my string"
"my string" :: [Char]

-}

printSecond :: IO ()
printSecond = do
  putStrLn greeting


myGreeting :: String
myGreeting = (++)"hello"  " world!"

hello :: String
hello = "Hello"

world :: String
world = "World!"

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5

area d = pi * (r * r)
  where r = d / 2


main :: IO ()
--main = putStrLn "Hello World!"
main = print $ rvrs "curry is awesome"
  --putStrLn myGreeting
  --putStrLn secondGreeting
  --where secondGreeting = (++) hello ((++)" " world)
  --putStrLn greeting
  --printSecond

greeting = "Yarrr"



addExclame str = str ++ "!"

getThirdElement :: [a] -> a
getThirdElement str = str !! 2

getIndexElement x =
  "curry is awesome" !! x


--  (drop 9 str) ++ (take 2 (drop 6 str)) ++ (take 5 str)


sps = " "

rvrs :: String -> String
rvrs str =
  drop 9 str ++ sps ++ take 2 (drop 6 str) ++ sps ++ take 5 str



-- 3.9 Definitions

--review as needed.
