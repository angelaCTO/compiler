{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.FDL.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import qualified Data.List          as L
import           Text.Printf        (printf)
import           Language.FDL.Types
import           Language.FDL.Utils
import           Control.Exception

--------------------------------------------------------------------------------
fromMaybe a i = case a of
  Just b ->  b
  Nothing -> i
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Check
--------------------------------------------------------------------------------
check :: Bare -> Bare
check p = case wellFormed p of
            [] -> p
            es -> throw es
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | `wellFormed e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormed :: Bare -> [UserError]
wellFormed = go emptyEnv
  where
    gos                       = concatMap . go
    go _    (Boolean {})      = []
    go _    (Number  n     l) = largeNumberErrors      n l
    go vEnv (Id      x     l) = unboundVarErrors  vEnv x l
    go vEnv (Prim1 _ e     _) = go  vEnv e
    go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]
    go vEnv (If   e1 e2 e3 _) = gos vEnv [e1, e2, e3]
    go vEnv (Let x e1 e2   _) = duplicateBindErrors vEnv x
                             ++ go vEnv e1
                             ++ go (addEnv x vEnv) e2
    go vEnv (Tuple es      _) = gos vEnv es
    go vEnv (GetItem e1 e2 _) = gos vEnv [e1, e2]
    go vEnv (App e es      _) = gos vEnv (e:es)
    go vEnv (Lam xs e      _) = duplicateParamErrors xs
                             ++ go (addsEnv xs vEnv) e
    go vEnv (Fun f xs e    _) = duplicateParamErrors xs
                             ++ go (addsEnv (f:xs) vEnv) e
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | addsEnv
--------------------------------------------------------------------------------
addsEnv :: [BareBind] -> Env -> Env
addsEnv xs env = L.foldl' (\env x -> addEnv x env) env xs
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
{- *This is no longer needed (covered by shadow binding checks)
duplicateFunErrors :: [BareDecl] -> [UserError]
duplicateFunErrors = fmap errDupFun . concat . dupBy (bindId . fName)
-}


duplicateParamErrors :: [BareBind] -> [UserError]
duplicateParamErrors xs
  = map errDupParam
  . map head
  . dupBy bindId
  $ xs


duplicateBindErrors :: Env -> BareBind -> [UserError]
duplicateBindErrors vEnv x
  = condError (memberEnv (bindId x) vEnv) (errDupBind x)


largeNumberErrors :: Integer -> SourceSpan -> [UserError]
largeNumberErrors n l
  = condError (maxInt < abs n) (errLargeNum l n)


maxInt :: Integer
maxInt = 1073741824


unboundVarErrors :: Env -> Id -> SourceSpan -> [UserError]
unboundVarErrors vEnv x l
  = condError (not (memberEnv x vEnv)) (errUnboundVar l x)


unboundFunErrors :: Env -> Id -> SourceSpan -> [UserError]
unboundFunErrors fEnv f l =
    condError ((memberEnv f fEnv)) (errUnboundFun l f)


callArityErrors :: Env -> Id -> SourceSpan -> Int -> [UserError]
callArityErrors fEnv f l i =
    condError (((fromMaybe (lookupEnv f fEnv) i) == i)) (errCallArity l f)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Error Constructors
--------------------------------------------------------------------------------
condError :: Bool -> UserError -> [UserError]
condError True  e = [e]
condError False _ = []

errDupParam     x = mkError (printf "Duplicate parameter '%s'" (bindId x)) (sourceSpan x)
errDupBind      x = mkError (printf "Shadow binding '%s'" (bindId x))      (sourceSpan x)
errLargeNum   l n = mkError (printf "Number '%d' is too large" n) l
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l
errUnboundFun l f = mkError (printf "Function '%s' is not defined" f) l
errUnboundFun l f = mkError (printf "Function '%s' is not defined" f) l
errCallArity  l f = mkError (printf "Wrong arity of arguments at call of %s" f) l
{- No longer needed
  errDupFun d = mkError (printf "duplicate function '%s'" (pprint f)) (sourceSpan f) where f = fName d
-}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
