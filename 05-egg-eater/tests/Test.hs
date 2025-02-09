{-# LANGUAGE ScopedTypeVariables #-}
-- module Test where 

import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit
import Language.Egg.Runner 
import Language.Egg.Types      hiding (Result)

main :: IO ()
main = do
  sc <- initScore
  defaultMain (tests sc) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> TestTree
tests sc = testGroup "Tests"
  [ testGroup "Normalizer"      (anfTests     sc)
  , testGroup "Adder"           (adderTests   sc)
  , testGroup "Boa"             (boaTests     sc)
  , testGroup "Cobra"           (cobraTests   sc)
  , testGroup "Diamond"         (diamondTests sc)
  , testGroup "Egg-eater"       (eggTests     sc)
  , testGroup "Dynamic-Errors"  (dynamicTests sc)
  , testGroup "Static-Errors"   (staticTests  sc)
  , testGroup "Your-Tests"      (yourTests    sc)
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

cobraTests sc =
  [ mkTest sc "neg00"       File      (rLines ["-3"])
  , mkTest sc "neg01"       File      (rLines ["-2"])
  , mkTest sc "print0"      File      (rLines ["12", "12"])
  , mkTest sc "print1"      File      (rLines ["true", "true"])
  , mkTest sc "print2"      File      (rLines ["false", "false"])
  , mkTest sc "print3"      File      (rLines ["2", "4", "4"])
  , mkTest sc "bool0"       File      (rLines ["false", "true"])
  , mkTest sc "bool1"       File      (rLines ["10", "true"])
  , mkTest sc "bool2"       File      (rLines ["6", "false", "false"])
  , mkTest sc "bool3"       File      (rLines ["44"])
  , mkTest sc "bool4"       File      (rLines ["100", "true", "200"])
  , mkTest sc "dyn0"        File      (rLines ["120"])
  , mkTest sc "dyn1"        File      (rLines ["13", "130"])
  , mkTest sc "dyn2"        File      (rLines ["144"])
  ]

staticTests sc =
  [ mkTest sc "err-shadow-bind"  File  (staticError "shadow binding")
  , mkTest sc "err-arity"        File  (staticError "arity")
  , mkTest sc "err-not-defined"  File  (staticError "not defined")
  , mkTest sc "err-unbound-00"   File  (staticError "unbound")
  , mkTest sc "err-unbound-01"   File  (staticError "unbound")
  , mkTest sc "err-dup-param"    File  (staticError "duplicate parameter")
  , mkTest sc "err-dup-fun"      File  (staticError "duplicate function")
  , mkTest sc "err-large-00"    File  (staticError "too large")
  , mkTest sc "err-large-01"      File  (staticError "too large")
  ]

dynamicTests sc =
  [ mkTest sc "add-l"    File  (dynamicError TNumber)
  , mkTest sc "add-r"    File  (dynamicError TNumber)
  , mkTest sc "sub-l"    File  (dynamicError TNumber)
  , mkTest sc "sub-r"    File  (dynamicError TNumber)
  , mkTest sc "mul-l"    File  (dynamicError TNumber)
  , mkTest sc "mul-r"    File  (dynamicError TNumber)
  , mkTest sc "lt-l"     File  (dynamicError TNumber)
  , mkTest sc "lt-r"     File  (dynamicError TNumber)
  , mkTest sc "gt-l"     File  (dynamicError TNumber)
  , mkTest sc "gt-r"     File  (dynamicError TNumber)
  , mkTest sc "add1-e"   File  (dynamicError TNumber)
  , mkTest sc "sub1-e"   File  (dynamicError TNumber)
  , mkTest sc "if1"      File  (dynamicError TBoolean)
  , mkTest sc "if2"      File  (dynamicError TBoolean)
  , mkTest sc "oflow00"  File  overflowError
  , mkTest sc "oflow01"  File  overflowError
  ]

diamondTests sc =
  [ mkTest sc "abs"       File      (rLines ["0", "5", "7", "0"])
  , mkTest sc "incr"      File      (rLines ["6"])
  , mkTest sc "add2"      File      (rLines ["19"])
  , mkTest sc "fac"       File      (rLines ["5", "4", "3", "2", "1", "0", "120"])
  , mkTest sc "fac-tr"    File      (rLines ["120"])
  , mkTest sc "fib"       File      (rLines ["1", "1", "2", "3", "5", "8", "0"])
  , mkTest sc "even"      File      (rLines ["true", "false", "true", "false", "0"])
  ]

eggTests sc =
  [ mkTest sc "dyn3"      File      (rLines ["true"])
  , mkTest sc "dyn4"      File      (rLines ["false"])
  , mkTest sc "tuple-00"  File      (rLines ["20"])
  , mkTest sc "tuple-01"  File      (rLines ["10", "20", "60"])
  , mkTest sc "tuple-02"  File      (rLines ["(10, 20, 30)"])
  , mkTest sc "tuple-03"  File      (rLines ["(10, 20, 30)", "(30, 20, 10)"])
  , mkTest sc "tuple-04"  File      (rLines ["(10, 20, 30)", "(30, 20, (10, 20, 30))"])
  , mkTest sc "tup-eq-0"  File      (rLines ["true"])
  , mkTest sc "tup-eq-1"  File      (rLines ["false"])
  , mkTest sc "list-0"    File      (rLines ["(0, (1, (2, (3, (4, (5, false))))))"])
  , mkTest sc "list-1"    File      (rLines ["6"])
  , mkTest sc "list-2"    File      (rLines ["(5, (4, (3, (2, (1, (0, false))))))"])
  , mkTest sc "list-3"    File      (rLines ["(0, (1, (2, (3, (4, (5, false))))))"])
  , mkTest sc "list-4"    File      (rLines ["(0, (1, (2, (3, (4, (5, false))))))"])
  ]

yourTests sc = 
  [ mkTest sc "LowAccess_outOfBounds"  File (Left  "Error: tuple index too small."),
    mkTest sc "HighAccess_outOfBounds" File (Left  "Error: tuple index too large."),
    mkTest sc "Access_okayBounds"      File (Right "3"),
    mkTest sc "NestedAccess_okayBounds"File (Right "3"),
    mkTest sc "AccessLet"              File (Right "1"),
    mkTest sc "Access_BadAnf"          File (Left  "Error: tuple index too large."),
    mkTest sc "Access_BadAnf2"         File (Right "5"),
    mkTest sc "AccessFalse"            File (Right "false"),
    mkTest sc "AccessSomething"        File (Right "9"),
    mkTest sc "getTrue"                File (Right "20")
  ]
