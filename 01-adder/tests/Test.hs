{-# LANGUAGE ScopedTypeVariables #-}

import           Common
import           Control.Exception
import           System.Exit
import           Test.Tasty
import           Text.Printf

main :: IO ()
main = do
  sc <- initScore
  defaultMain (tests sc) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> TestTree
tests sc = testGroup "Tests"
  [ testGroup "Adder"           (adderTests   sc)
  , testGroup "Your-Tests"      (yourTests    sc)
  ]

adderTests sc =
  [ mkTest sc "forty_one"  (Code "41")               (Right "41")
  , mkTest sc "nyi"        (Code "let x = 10 in x")  (Right "10")
  , mkTest sc "five"        File                     (Right "5")
  , mkTest sc "adds"        File                     (Right "8")
  , mkTest sc "subs"        File                     (Right "8")
  , mkTest sc "lets"        File                     (Right "14")
  , mkTest sc "lets-unb"    File                     (Left  "")
  ]

--------------------------------------------------------------------------------
-- | BEGIN yourTest DO NOT EDIT THIS LINE --------------------------------------
--------------------------------------------------------------------------------

yourTests sc =
  [ mkTest sc "lots_of_lets" (Code "let x = 10 in let y = x in let z = y in z") (Right "10"),
    mkTest sc "negative" (Code "sub1(sub1(sub1(2)))") (Right "-1"),
    mkTest sc "large_let" (Code " let x = 10, y = 11 in let z = add1(y) in sub1(add1(add1(x)))") (Right "11"),
    mkTest sc "mult_define" (Code "let a = 10, b = 11 in let x = add1(a), y = sub1(b) in y") (Right "10"),
    mkTest sc "no_effect" (Code "let a = 1 in let b = add1(a), c = sub1(b), d = add1(add1(a)) in let y = sub1(d) in a") (Right "1")
  ]
