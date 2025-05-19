{-# LANGUAGE GHC2021 #-}

module Church
  ( Church.and
  , Church.or
  , Church.xor
  , Church.not
  , ifThenElse
  , fromHaskellBool
  , toHaskellBool
  , true
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

not :: ChurchBool -> ChurchBool
not b x y = b y x

and :: ChurchBool -> ChurchBool -> ChurchBool
and a b x y = a (b x y) y

or :: ChurchBool -> ChurchBool -> ChurchBool
or a b x y = a x (b x y)

xor :: ChurchBool -> ChurchBool -> ChurchBool
xor a b x y = a (b y x) (b x y)

ifThenElse :: ChurchBool -> a -> a -> a
ifThenElse b = b

toHaskellBool :: ChurchBool -> Bool
toHaskellBool b = b True False

fromHaskellBool :: Bool -> ChurchBool
fromHaskellBool True = true
fromHaskellBool False = false

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

pair :: a -> b -> Pair a b
pair a b f = f a b

fst :: Pair a b -> a
fst p = p const

snd :: Pair a b -> b
snd p = p (\_ b -> b)
