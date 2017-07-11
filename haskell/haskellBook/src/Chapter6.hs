module Chapter6 where

-- Typeclasses

-- Typeclasses allow us to generalize over a set of types in order to define and execute a standard set of features for those types.


-- keep your typeclass instances for a type
-- in the same file as the type
-- we'll explain why later

data Mood = Blah
instance Show Mood where
  show _ = "Blah"

-- add :: a -> a -> a Wont compile
-- add :: Num a => a -> a -> a       --will compile
add x y =
  x + y


-- addWeird :: Num a => a -> a -> a        won't compile
-- addWeird :: (Ord a, Num a) => a -> a -> a   - will compile
addWeird x y =
  if x > 1
  then x + y
  else x

data Trivial =
  Trivial'     -- data contructor

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Ord, Show)

-- day of week and numerical day of month

data Date =
  Date DayOfWeek
       Int

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'


f :: Int -> Bool
f 1 = True
f _ = False

data Identity a =
  Identity a
{-
instance Eq a => Eq (Identity a) where (==) (Identity v) (Identity v')
  = v == v'
-}
instance Ord a => Eq (Identity a) where
  (==) (Identity v) (Identity v') =
    compare v v' == EQ
-- This will compile but it's unclear why you would do it. ????

--Exercises: Eq Instances
--1. provided:

data TisAnInteger =
  TisAn Integer

--my instance:

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

{-

λ> TisAn 3 == TisAn 3
True

-}

--2. provided:

data TwoIntegers =
  Two Integer Integer

--my instance:

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') = v1 == v1' && v2 == v2'

{-

λ> Two 3 4 == Two 3 4
True

-}

--3. provided:

data StringOrInt
  = TisAnInt Int
  | TisAString String

-- my instance:

instance Eq StringOrInt where
  (==) (TisAnInt v)(TisAnInt v')      = v == v'
  (==) (TisAString v) (TisAString v') = v == v'
  (==)  _                _            = False

{-

λ> TisAString "Cat" == TisAString "Cat"
True

λ> TisAnInt 42 == TisAnInt 42
True

-}

-- need to comeback to typeclass later. .. got bored
