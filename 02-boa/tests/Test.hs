{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit

main :: IO ()
main = do
  sc <- initScore
  defaultMain (tests sc) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> TestTree
tests sc = testGroup "Tests"
  [ 
    testGroup "Normalizer"      (anfTests     sc),
    testGroup "Adder"           (adderTests   sc),
    testGroup "Boa"             (boaTests     sc),
    testGroup "Your-Tests"      (yourTests    sc)
  ]

anfTests sc =
  [ anfTest sc "prim1"
      "add1(add1(add1(add1(x))))"
      "(let anf0 = add1(x), anf1 = add1(anf0), anf2 = add1(anf1) in add1(anf2))"

  , anfTest sc "prim2"
      "((2 + 3) * (12 - 4)) * (7 + 8)"
      "(let anf0 = 2 + 3, anf1 = 12 - 4, anf2 = anf0 * anf1, anf3 = 7 + 8 in anf2 * anf3)"

  , anfTest sc "let-1"
      "(let x = 10 in x + 5) + (let y = 20 in y - 5)"
      "(let anf0 = (let x = 10 in x + 5), anf1 = (let y = 20 in y - 5) in anf0 + anf1)"

  , anfTest sc "if-1"
      "(if x: y + 1 else: z + 1) + 12"
      "(let anf0 = (if x: y + 1 else: z + 1) in anf0 + 12)"
  ]

adderTests sc =
  [ mkTest sc "forty_one"  (Code "41")               (Right "41")
  , mkTest sc "nyi"        (Code "let x = 10 in x")  (Right "10")
  , mkTest sc "five"        File                     (Right "5")
  , mkTest sc "adds"        File                     (Right "8")
  , mkTest sc "subs"        File                     (Right "8")
  , mkTest sc "lets"        File                     (Right "14")
  , mkTest sc "expr0"       File                     (Right "600")
  ]

boaTests   sc =
  [ mkTest sc "expr1"       File      (Right "30")
  , mkTest sc "expr2"       File      (Right "20")
  , mkTest sc "expr3"       File      (Right "20")
  , mkTest sc "expr4"       File      (Right "-8")
  , mkTest sc "exp00"       File      (Right "65536")
  ]

yourTests sc =
  [ 
    mkTest sc "badVar" File (Left (unboundVarString "x")),
    mkTest sc "badLet" File (Left (unboundVarString "y")),
    mkTest sc "fiveLets" File (Right "5"),
    mkTest sc "big_test" (Code "let z = sub1(5), y = sub1(z) in sub1(add1(add1(y)))") (Right "4"),
    mkTest sc "ignored_let_right" (Code "let z = sub1(5), y = sub1(z), l = 5 in sub1(add1(add1(y)))") (Right "4"),
    mkTest sc "ignored_let_left" (Code "let l = 5, z = sub1(5), y = sub1(z) in sub1(add1(add1(y)))") (Right "4"),
    mkTest sc "shadow" (Code "let z = sub1(5), y = sub1(z) in let y = 1 in y") (Right "1"),
    mkTest sc "let_in_add" (Code "add1(let x = 8 in add1(x))") (Right "10"),
    mkTest sc "yOutOfScope" File (Left (unboundVarString "y")),
    mkTest sc "bigAnfTest" File (Right $ show (((1+1)+(1+1))*((1+1)+(1+1)))),
    mkTest sc "ifThenAnf" File (Right $ show $ 2+2+2-1),
    mkTest sc "letInAdd2" (Code "(let x = 8 in add1(x))+(let x = 4 in sub1(x))") (Right "12"),
    mkTest sc "aTest" File (Right "72"),
    mkTest sc "multTest" File (Right "100"),
    mkTest sc "longAdd" File (Right "36"),
    mkTest sc "longSub" File (Right "36"),
    mkTest sc "longAddMult" File (Right "0"),
    mkTest sc "ifinif" (Code "if 5: if 6: 1 else: 0 else: 4") ( Right "1" ),
    mkTest sc "letInIf" (Code "if let x = 1, y = 1+2+3, z = x + y + 2 in z:let a = 2 + 2 + 2 in a + 2 else: let d = 3 in d") (Right "8")
  ]

unboundVarString :: String -> String
unboundVarString var = printf "Unbound variable '%s'" var
undefinedString = error "Test Invalid: Tester must fill in the correct string here!"

