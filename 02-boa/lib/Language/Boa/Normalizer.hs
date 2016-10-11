--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.Boa.Normalizer ( anormal ) where

import           Language.Boa.Types

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

anf i (Id     x l)      = (i, Id     x l)

-- TODO
--Need to check this again, but I think a "let" expressions are in ANF form ...
--anf i (Let x e b l)     = error "TBD:anf:let"
anf i (Let x e b l)     = (i, Let x e b l)

anf i (Prim1 o e l)     = (i', stitch bs  (Prim1 o ae l))
  where
    (i', bs, ae)        = imm i e

-- TODO
anf i (Prim2 o e1 e2 l) = error "TBD:anf:prim2"

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--   NOTE: the binders are in reverse order.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr [ (x, xe) | (x, (xe, _)) <- reverse bs] e (getLabel e)

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
-- TODO
imm i (Number n l)      = error "TBD:imm:Number"

-- TODO
imm i (Id x l)          = error "TBD:imm:Id"

imm i (Prim1 o e1 l)    = (i'', bs, mkId v l)
  where
    (i' , b1s, v1)      = imm i e1
    (i'', v)            = fresh l i'
    bs                  = (v, (Prim1 o v1 l, l)) : b1s

-- TODO
imm i (Prim2 o e1 e2 l) = error "TBD:imm:prim2"

imm i e@(If _ _ _  l)   = immExp i e l

imm i e@(Let _ _ _ l)   = immExp i e l


immExp :: Int -> AnfExpr a -> a -> (Int, Binds a, ImmExpr a)
immExp i e l  = (i'', bs, mkId v l)
  where
    (i' , e') = anf i e
    (i'', v)  = fresh l i'
    bs        = [(v, (e', l))]

mkId :: Bind a -> a -> Expr a
mkId x l = Id (bindId x) l

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
fresh :: a -> Int -> (Int, Bind a)
--------------------------------------------------------------------------------
fresh l i = (i + 1, Bind x l)
  where
    x     = "anf" ++ show i
