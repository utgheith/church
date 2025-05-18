{-# LANGUAGE RankNTypes #-}

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
type ChurchBool = forall a. a -> a -> a

true :: ChurchBool
true a _ = a

false :: ChurchBool
false _ a = a

-- Church Nat
type Nat = forall x. (x -> x) -> x -> x

zero :: Nat
zero _ x = x

next :: Nat -> Nat
next n f x = f (n f x)

add :: Nat -> Nat -> Nat
add n m f x = m f (n f x)

isZero :: Nat -> ChurchBool
isZero n it_is_zero it_is_not_zero = n (const it_is_not_zero) it_is_zero

toHaskell :: Nat -> Natural
toHaskell c = c (+ 1) (0 :: Natural)

fromHaskell :: Natural -> Nat
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
type Pair a b = forall c. (a -> b -> c) -> c

pair :: forall a b. (a -> b -> Pair a b)
pair a b f = f a b

fst :: forall a b. (Pair a b -> a)
fst p = p const

snd :: forall a b. (Pair a b -> b)
snd p = p (\_ b -> b)
