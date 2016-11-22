--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.FDL.Normalizer ( anormal ) where

import           Language.FDL.Types

type Binds a = [(Bind a, (AnfExpr a, a))]

--------------------------------------------------------------------------------
-- | Convert an Expr into A-Normal Form
--------------------------------------------------------------------------------
anormal :: Expr a -> AnfExpr a
--------------------------------------------------------------------------------
anormal e = snd (anf 0 e)

--------------------------------------------------------------------------------
-- | `anf i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `e'` is equivalent to `e` but is in A-Normal Form.
--------------------------------------------------------------------------------
anf :: Int -> Expr a -> (Int, AnfExpr a)
--------------------------------------------------------------------------------
anf i (Number n l)      = (i, Number n l)

anf i (Boolean b l)     = (i, Boolean b l)

anf i (Id     x l)      = (i, Id     x l)

anf i (Let x e b l)     = (i'', Let x e' b' l)
  where
    (i',  e')           = anf i e
    (i'', b')           = anf i' b

anf i (Prim1 o e l)     = (i', stitch bs  (Prim1 o ae l))
  where
    (i', bs, ae)        = imm i e

anf i (Prim2 o e1 e2 l) = (i'', stitch bs'' (Prim2 o e1' e2' l))
  where
    bs''                = bs' ++ bs
    (i' , bs , e1')     = imm i  e1
    (i'', bs', e2')     = imm i' e2

anf i (If c e1 e2 l)    = (i''', stitch bs  (If c' e1' e2' l))
  where
    (i'  , bs, c')      = imm i   c
    (i'' ,     e1')     = anf i'  e1
    (i''',     e2')     = anf i'' e2

anf _i (Tuple _es _l)   = (i', stitch bs (Tuple es' _l))
  where
    (i', bs, es')       = imms _i _es


anf _i (GetItem _e1 _e2 _l) = (i'', stitch bs' (GetItem e1' e2' _l))
  where
    (i',  bs1, e1')       = imm _i  _e1
    (i'', bs2, e2')       = imm i'  _e2
    bs'                   = bs2 ++ bs1

anf _i (App _e _es _l)    = (i', stitch bs (App e' es' _l))
  where
    (i', bs, e':es')      = imms _i (_e : _es)

anf _i (Lam _xs _e _l)    = (i', Lam _xs a _l)
  where
    (i', a)               = anf _i _e

anf _i (Fun _f _xs _e _l) = (i', Fun _f _xs a _l)
   where
    (i', a)               = anf _i a

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr [ (x, e) | (x, (e, _)) <- reverse bs] e (getLabel e)

--------------------------------------------------------------------------------
-- | `imms i es` takes as input a "start" counter `i` and expressions `es`, and
--   and returns an output `(i', bs, es')` where
--   * `i'` is the output counter (i.e. i'- i) anf-variables were generated
--   * `bs` are the temporary binders needed to convert `es` to immediate vals
--   * `es'` are the immediate values  equivalent to es
--------------------------------------------------------------------------------
imms :: Int -> [AnfExpr a] -> (Int, Binds a, [ImmExpr a])
--------------------------------------------------------------------------------
imms i []           = (i, [], [])
imms i (e:es)       = (i'', bs' ++ bs, e' : es' )
  where
    (i' , bs , e' ) = imm  i  e
    (i'', bs', es') = imms i' es

--------------------------------------------------------------------------------
-- | `imm i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', bs, e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `bs` are the temporary binders needed to render `e` in ANF, and
--   * `e'` is an `imm` value (Id or Number) equivalent to `e`.
--------------------------------------------------------------------------------
imm :: Int -> AnfExpr a -> (Int, Binds a, ImmExpr a)
--------------------------------------------------------------------------------
imm i (Number n l)      = (i  , [], Number n l)

imm i (Boolean b  l)    = (i  , [], Boolean b l)

imm i (Id x l)          = (i  , [], Id x l)

imm i (Prim1 o e1 l)    = (i'', bs, mkId v l)
  where
    (i' , b1s, v1)      = imm i e1
    (i'', v)            = fresh l i'
    bs                  = (v, (Prim1 o v1 l, l)) : b1s

imm i (Prim2 o e1 e2 l) = (i'', bs', mkId x l)
  where
    (i', bs, [v1, v2])  = imms i [e1, e2]
    (i'', x)            = fresh l i'
    bs'                 = (x, (Prim2 o v1 v2 l, l)) : bs

imm _i (Tuple _es _l)      = (i'', bs', mkId x _l)
  where
    (i', bs, vs)        = imms _i _es
    (i'', x)            = fresh _l i'
    bs'                 = (x, (Tuple _es _l, _l)) : bs

imm _i (GetItem _e1 _e2 _l) = (i''', bs', mkId x _l)
  where
    (i', b1', e1')      = imm _i _e1
    (i'', b2', e2')     = imm i' _e2
    (i''', x)           = fresh _l i''
    bs'                 = (x, (GetItem e1' e2' _l, _l)) : b2'++b1'

imm i e@(If _ _ _  l)   = immExp i e l

imm i e@(Let _ _ _ l)   = immExp i e l


imm _i (App _e _es _l)  = (i'', bs', mkId x _l)
  where
    (i', bs, es')       = imms _i _es
    (i'', x)            = fresh _l i'
    bs'                 = (x, (App _e es' _l, _l)) : bs

imm i e@(Lam _ _ l)     = immExp i e l

imm i e@(Fun _ _ _ l)   = immExp i e l

immExp :: Int -> AnfExpr a -> a -> (Int, Binds a, ImmExpr a)
immExp i e l  = (i'', bs, mkId v l)
  where
    (i' , e') = anf i e
    (i'', v)  = fresh l i'
    bs        = [(v, (e', l))]

mkId :: Bind a -> a -> Expr a
mkId x = Id (bindId x)

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
fresh :: a -> Int -> (Int, Bind a)
--------------------------------------------------------------------------------
fresh l i = (i + 1, Bind x l)
  where
    x     = "anf" ++ show i
