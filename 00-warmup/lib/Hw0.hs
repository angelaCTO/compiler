{-  | Name:    Angela To              A10657395
    | Partner: Christopher Shmerling  A12415880
    | Date:    09/26/2016
    |
    | CSE 131: FP Refresher/Intro to Haskell Assignment.
     Do not change the skeleton code! The point of this
     assignment is to figure out how the functions can
     be written this way (using fold). You may only
     replace the `error "TBD:..."` parts.

     For this assignment, you may use the following library functions:

     map
     foldl'
     foldr
     length
     append
 -}

module Hw0 where
import Prelude  hiding (replicate, sum, reverse)
import Data.List (foldl')


-- SUM_LIST----------------------------------------------------------------------
-- | Sum the elements of a list
--
-- >>> sumList [1, 2, 3, 4]
-- 10
--
-- >>> sumList [1, -2, 3, 5]
-- 7
--
-- >>> sumList [1, 3, 5, 7, 9, 11]
-- 36
sumList :: [Int] -> Int
sumList []    = 0
sumList(x:xs) = x + sumList xs
--sumList xs = foldl (+) 0 xs
---------------------------------------------------------------------------------


-- DIGITS_OF_INT ----------------------------------------------------------------
-- | `digitsOfInt n` should return `[]` if `n` is not positive,
--    and otherwise returns the list of digits of `n` in the
--    order in which they appear in `n`.
--
-- >>> digitsOfInt 3124
-- [3, 1, 2, 4]
--
-- >>> digitsOfInt 352663
-- [3, 5, 2, 6, 6, 3]

digitsOfInt :: Int -> [Int]
digitsOfInt n
    | n <= 0  = []
    | n > 0   = digitsOfInt(n `div` 10) ++ [n `mod` 10]
---------------------------------------------------------------------------------


-- DIGITS -----------------------------------------------------------------------
-- | `digits n` retruns the list of digits of `n`
--
-- >>> digits 31243
-- [3,1,2,4,3]
--
-- digits (-23422)
-- [2, 3, 4, 2, 2]

digits :: Int -> [Int]
digits n = digitsOfInt (abs n)
---------------------------------------------------------------------------------


-- SUM_INT (HELPER FUNCTION) ----------------------------------------------------
-- | Sums the digits passed integer
--
-- >>> sumInt(1234)
-- 10
sumInt :: Int -> Int
sumInt n
    | n `div` 10 <= 0 = n
    | n `div` 10 > 0  = (n `mod` 10) + sumInt(n `div` 10)


-- ADDITVE_PERSISTENCE-----------------------------------------------------------
-- | From http://mathworld.wolfram.com/AdditivePersistence.html
--   Consider the process of taking a number, adding its digits,
--   then adding the digits of the number derived from it, etc.,
--   until the remaining number has only one digit.
--   The number of additions required to obtain a single digit
--   from a number n is called the additive persistence of n,
--   and the digit obtained is called the digital root of n.
--   For example, the sequence obtained from the starting number
--   9876 is (9876, 30, 3), so 9876 has
--   an additive persistence of 2 and
--   a digital root of 3.
--
-- NOTE: assume additivePersistence & digitalRoot are only called with positive
-- numbers
--
-- >>> additivePersistence 9876
-- 2
additivePersistence :: Int -> Int
additivePersistence n
    | sumInt(n) < 9  = 1
    | sumInt(n) >= 9 = 1 + additivePersistence(sumInt(n))
---------------------------------------------------------------------------------


-- DIGITAL_ROOT -----------------------------------------------------------------
-- | digitalRoot n is the digit obtained at the end of the sequence
--   computing the additivePersistence
--
-- >>> digitalRoot 9876
-- 3
digitalRoot :: Int -> Int
digitalRoot n
    | n < 9  = n
    | n >= 9 = digitalRoot(sumInt(n))
---------------------------------------------------------------------------------



-- LIST_REVERSE -----------------------------------------------------------------
-- | listReverse [x1,x2,...,xn] returns [xn,...,x2,x1]
--
-- >>> listReverse []
-- []
--
-- >>> listReverse [1,2,3,4]
-- [4,3,2,1]
--
-- >>> listReverse ["i", "want", "to", "ride", "my", "bicycle"]
-- ["bicycle", "my", "ride", "to", "want", "i"]

listReverse :: [a] -> [a]
listReverse []    = []
listReverse(x:xs) = listReverse(xs) ++ [x]
---------------------------------------------------------------------------------


-- PALINDROME -------------------------------------------------------------------
-- | In Haskell, a `String` is a simply a list of `Char`, that is:
--
-- >>> ['h', 'a', 's', 'k', 'e', 'l', 'l']
-- "haskell"
--
-- >>> palindrome "malayalam"
-- True
--
-- >>> palindrome "myxomatosis"
-- False

palindrome :: String -> Bool
palindrome w = do
    let w' = listReverse(w)
    w == w'
---------------------------------------------------------------------------------


-- FOLD_LEFT --------------------------------------------------------------------
foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft = foldl'
---------------------------------------------------------------------------------


-- SQ_SUM -----------------------------------------------------------------------
-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
   f a x = x * x + a
   base  = 0
---------------------------------------------------------------------------------


-- PIPE -------------------------------------------------------------------------
-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24
pipe :: [(a -> a)] -> (a -> a)
pipe fs   = foldLeft f base fs
  where
    f a x = \x' -> a(x x')
    base  = \x  -> x
---------------------------------------------------------------------------------


-- SEP_CONCAT -------------------------------------------------------------------
-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"

sepConcat :: String -> [String] -> String
sepConcat sep []    = ""
sepConcat sep (h:t) = foldLeft f base l
  where
    f a x           = a ++ sep ++ x
    base            = h
    l               = t
---------------------------------------------------------------------------------


--INT_STRING --------------------------------------------------------------------
intString :: Int -> String
intString = show
---------------------------------------------------------------------------------


-- STRING_OF_LIST ---------------------------------------------------------------
-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"

stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = "[" ++ (sepConcat ", " (map f xs)) ++ "]"
---------------------------------------------------------------------------------


-- CLONE ------------------------------------------------------------------------
-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]

clone :: a -> Int -> [a]
clone x n
    | n <= 0 = []
    | n > 0  = x : clone x (n - 1)
---------------------------------------------------------------------------------


-- PAD_ZERO ---------------------------------------------------------------------
-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- [0,0,9,9] [1,0,0,2]
--
-- >>> padZero [1,0,0,2] [9,9]
-- [1,0,0,2] [0,0,9,9]
type BigInt = [Int]
padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2
    | length(l1) > length(l2)  = (l1, clone 0 (length(l1)-length(l2)) ++ l2)
    | length(l1) < length(l2)  = (clone 0 (length(l2)-length(l1)) ++ l1, l2)
    | length(l1) == length(l2) = (l1, l2)
---------------------------------------------------------------------------------


-- REMOVE_ZERO ------------------------------------------------------------------
-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []

removeZero :: BigInt -> BigInt
removeZero [] = []
removeZero (h:t)
    | h == 0    = removeZero t
    | otherwise = h:t
---------------------------------------------------------------------------------


-- BIG_ADD ----------------------------------------------------------------------
-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]

add :: (BigInt, BigInt) -> Int -> BigInt -> BigInt
add (l1', l2') carry res =  case (l1', l2') of
    ([],[])           -> removeZero(carry:res)
    ((h1:t1),(h2:t2)) -> add (t1, t2) ((carry + h1 + h2) `div` 10) (((h1 + h2 + carry) `mod` 10) : res)


bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2 = add tup' 0 []
    where
        tup = padZero l1 l2
        tup' = (listReverse (fst tup), listReverse (snd tup))


--bigAdd l1 l2 = add (padZero(listReverse l1) (listReverse l2)) 0 []
---------------------------------------------------------------------------------


-- MUL_BY_DIGIT -----------------------------------------------------------------
-- | `mulByDigit i n` returns the result of multiplying
--   the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]

mult :: BigInt -> Int -> Int -> BigInt -> BigInt
mult l' i carry res = case l' of
    []    -> carry : res
    (h:t) -> mult t i ((carry + (h * i)) `div` 10) (((carry + (h*i)) `mod` 10) : res)


mulByDigit :: Int -> BigInt -> BigInt
mulByDigit 0 l = [0]
mulByDigit 1 l = l
mulByDigit i l = removeZero(mult(listReverse l) i 0 [])


---------------------------------------------------------------------------------


-- BIG_MUL ----------------------------------------------------------------------
-- | `bigMul n1 n2` returns the `BigInt` representinsg the product of `n1` and
-- `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1

bigMul :: BigInt -> BigInt -> BigInt
bigMul l1 l2 = res
  where
    (_, res) = foldLeft f base args
    f a x    = (fst a ++ [0], bigAdd (snd a) ((mulByDigit x l2) ++ (fst a)))
    base     = ([], [])
    args     = listReverse l1

---------------------------------------------------------------------------------


