module Church
  ( true
  , false
  , ChurchBool
  , zero
  , isZero
  , next
  , add
  , toHaskell
  , fromHaskell
  , Pair
  , pair
  , Church.fst
  , Church.snd
  ) where

import GHC.Natural

-- Church bools
type ChurchBool a b c = a -> b -> c

true :: ChurchBool a b a
true a _ = a

false :: ChurchBool a b b
false _ a = a

-- Church Nat
type Nat x = (x -> x) -> x -> x

zero :: Nat x
zero _ x = x

next :: Nat x -> Nat x
next n f x = f (n f x)

add :: Nat x -> Nat x -> Nat x
add n m f x = m f (n f x)

isZero :: Nat x -> ChurchBool x x x
isZero n it_is_zero it_is_not_zero = n (const it_is_not_zero) it_is_zero

toHaskell :: Nat Natural -> Natural
toHaskell c = c (+ 1) (0 :: Natural)

fromHaskell :: Natural -> Nat Natural
fromHaskell 0 = zero
fromHaskell x
  | x > 0 = o
  where
    h = fromHaskell $ div x 2
    d = add h h
    o =
      if mod x 2 == 1
        then next d
        else d
fromHaskell _ = error "negtive"

-- Pair
type Pair a b c = (a -> b -> c) -> c

pair :: a -> b -> Pair a b x
pair a b f = f a b

fst :: Pair a b a -> a
fst p = p true

snd :: Pair a b b -> b
snd p = p false
