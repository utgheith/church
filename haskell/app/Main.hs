module Main
  ( main
  ) where

import qualified Church as C

main :: IO ()
main = do
  putStrLn $ C.true "a" "b"
  putStrLn $ C.false "a" "b"
  print $ C.toHaskell C.zero
  let one = C.next C.zero
  print $ C.toHaskell one
  print $ C.toHaskell $ C.next one
  let two = C.next one
  let three = C.next two
  print (C.toHaskell $ C.add two three)
  print $ C.toHaskell $ C.add (C.fromHaskell 11) (C.fromHaskell 23)
  let p = C.pair (C.fromHaskell 11) (C.fromHaskell 23)
  print $ C.toHaskell $ C.fst p
  print $ C.toHaskell $ C.snd p
  print $ C.isZero C.zero "yes" "no"
  print $ C.isZero three "yes" "no"
