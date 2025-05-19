import qualified Church as C
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [boolTests, pairTests]

boolTests :: TestTree
boolTests =
  testGroup
    "Church Booleans"
    [ testCase "fromHaskellBool True"
        $ assertEqual "" (C.fromHaskellBool True 'a' 'b') 'a'
    , testCase "fromHaskellBool False"
        $ assertEqual "" (C.fromHaskellBool False 'a' 'b') 'b'
    , testCase "and true true"
        $ assertEqual "" (C.and C.true C.true 'a' 'b') 'a'
    , testCase "and true false"
        $ assertEqual "" (C.and C.true C.false 'a' 'b') 'b'
    , testCase "and false true"
        $ assertEqual "" (C.and C.false C.true 'a' 'b') 'b'
    , testCase "and false false"
        $ assertEqual "" (C.and C.false C.false 'a' 'b') 'b'
    , testCase "or true true" $ assertEqual "" (C.or C.true C.true 'a' 'b') 'a'
    , testCase "or true false"
        $ assertEqual "" (C.or C.true C.false 'a' 'b') 'a'
    , testCase "or false true"
        $ assertEqual "" (C.or C.false C.true 'a' 'b') 'a'
    , testCase "or false false"
        $ assertEqual "" (C.or C.false C.false 'a' 'b') 'b'
    , testCase "xor true true"
        $ assertEqual "" (C.xor C.true C.true 'a' 'b') 'b'
    , testCase "xor true false"
        $ assertEqual "" (C.xor C.true C.false 'a' 'b') 'a'
    , testCase "xor false true"
        $ assertEqual "" (C.xor C.false C.true 'a' 'b') 'a'
    , testCase "xor false false"
        $ assertEqual "" (C.xor C.false C.false 'a' 'b') 'b'
    ]

pairTests :: TestTree
pairTests =
  testGroup
    "Church Pairs"
    [ testCase "fst" $ assertEqual "" (C.fst (C.pair "1" (2 :: Int))) "1"
    , testCase "snd" $ assertEqual "" (C.snd (C.pair "1" (2 :: Int))) 2
    ]
