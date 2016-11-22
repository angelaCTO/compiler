{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.FDL.Compiler ( compiler, compile, compileEnv, countVars, freeVars ) where

import           Prelude                  hiding (compare)
import           Control.Arrow            ((>>>))
import           Data.Maybe
import           Data.Bits                       (shift)
import qualified Data.Set                as S
-- import           Language.FDL.Utils
import           Language.FDL.Types      hiding (Tag)
import           Language.FDL.Parser     (parse)
import           Language.FDL.Checker    (check, errUnboundVar)
import           Language.FDL.Normalizer (anormal)
import           Language.FDL.Asm        (asm)




--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
compiler f = parse f >>> check >>> anormal >>> tag >>> compile >>> asm
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag

--Added
type Arity = Arg --Int

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: AnfExpr SourceSpan -> AExp
tag = label
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.
--------------------------------------------------------------------------------
funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs = funEntry n ++ instrs ++ funExit


--Insert instructions for setting up stack for `n` local vars
funEntry :: Int -> [Instruction]
funEntry n = [ IPush (Reg EBP)                       -- save caller's ebp
             , IMov  (Reg EBP) (Reg ESP)             -- set callee's ebp
             , ISub  (Reg ESP) (Const (4 * n))       -- allocate n local-vars
             , IAnd  (Reg ESP) (HexConst 0xFFFFFFF0)]-- MacOS stack alignment


--Clean up stack & labels for jumping to error
funExit :: [Instruction]
funExit   = [ IMov (Reg ESP) (Reg EBP)          -- restore callee's esp
            , IPop (Reg EBP)                    -- restore callee's ebp
            , IRet]                             -- jump back to caller
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.s
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
countVars (Let _ e b _)  = max (countVars e)  (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0
--------------------------------------------------------------------------------
freeVars :: Expr a -> [Id]
freeVars = error "TBD:freeVars"
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--TODO Check Typing Declaration. not sure if correct
{-                f           xs         e            output
compileDecl :: (Bind a) -> [Bind a] -> (Expr a) -> [Instruction]
compileDecl f xs e  = ILabel (Builtin (bindId f)) : compileBody env e
  where 
     env            = fromListEnv (zip (bindId <$> xs) [-2, -3..])
-}
compileDecl :: [Bind a] -> (Expr a) -> [Instruction]
compileDecl xs e  = ILabel (Builtin (bindId f)) : compileBody env e
  where 
    env             = fromListEnv (zip (bindId <$> xs) [-2, -3..])


--TODO Check
compileBody :: Env -> AExp -> [Instruction]
compileBody env v = funInstrs (countVars v) (compileEnv env v)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Compile AExp Expressions
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
compile e@(Number  n l)      = compileBody emptyEnv e
                               

compile e@(Boolean b l)      = compileBody emptyEnv e 


compile e@(Id i l)           = compileBody emptyEnv e

compile e@(Prim1 p1 e1 l)    = compileBody emptyEnv e ++ 
                               concatMap compileDecl xs 

compile e@(Prim2 p2 e1 e2 l) = compileBody emptyEnv e ++ 
                               concatMap compileDecl xs


compile e@(If e1 e2 e3 l)    = compileBody emptyEnv e ++ 
                               concatMap compileDecl xs 


compile e@(Let b1 e1 e2 l)   = compileBody emptyEnv e ++ 
                               concatMap compileDecl xs 


compile e@(Tuple es l)       = compileBody emptyEnv e ++ 
                               concatMap compileDecl xs 


compile e@(GetItem e1 e2 l)  = compileBody emptyEnv e ++ 
                               concatMap (compileDecl e1 e2)


compile e@(App e1 es l)      = compileBody emptyEnv e ++ 
                               concatMap (compileDecl _ es e1) --Not sure


compile e@(Lam bs e1 l)      = compileBody emptyEnv e ++ 
                               concatMap (compileDecl _ bs e1)


compile e@(Fun b1 bs e1 l)   = compileBody emptyEnv e ++ 
                               concatMap (compileDecl b1 bs e1) 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- CompileEnv
--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env v@(Number {})        = [ compileImm env v  ]

compileEnv env v@(Boolean {})       = [ compileImm env v  ]

compileEnv env v@(Id {})            = [ compileImm env v  ]

compileEnv env e@(Let {})           = is ++ compileEnv env' body
  where
    (env', is)                      = compileBinds env [] binds
    (binds, body)                   = exprBinds e

compileEnv env (Prim1 o v l)        = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l)    = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)       = assertType env v TBoolean
                                   ++ IMov (Reg EAX) (immArg env v)
                                    : ICmp (Reg EAX) (repr False)
                                    : branch l IJe i1s i2s
  where
    i1s                             = compileEnv env e1
    i2s                             = compileEnv env e2

compileEnv _env (Tuple _es _)       = tupleAlloc  (length _es)          ++
                                      tupleWrites (immArg _env <$> _es) ++
				      [IOr (Reg EAX) (typeTag TTuple)]

compileEnv _env (GetItem _vE _vI _) = tupleRead _env _vE _vI

--compileEnv :: Env -> AExp -> [Instruction]
compileEnv env (Lam xs e l) 	    = IJmp end		    :              
  			              ILabel start          :      
  				      compileDecl l xs e    ++
				      ILabel end            :
  				      lamTuple arity start --compile fun-tuple 
                         where                             --into EAX
      			      arity = length xs
      			      start = LamStart l
      			      end   = LamEnd   l


compileEnv env (Fun f xs e l)       = IJmp   end           		:
                                      ILabel start                      :
                                      compileDecl l xs e                ++
                                      ILabel end                        :
                                      funTuple arity start --TODO Check
                         where
                               arity = length xs
                               start = (DefStart f l)
                               end   = (DefEnd   f l)

compileEnv env (App v vs l)    = assertType env vs TClosure                 ++
                                 assertArity env vs (length vs)             ++
                                 tupleReadRaw (immArg env v) (repr(1::Int)) ++
                                 [IPush (param env v) | v <- reverse vs]    ++
                                 [ICall (Reg EAX)]                          ++
                                 [IAdd (Reg ESP) (4 * n)]
                         where
                              n = countVars(v)




compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is ++ is') bs
  where
    (env', is')            = compileBind env b

compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      ++ [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env

compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1   v = compilePrim2 l env Plus  v (Number 1 l)
compilePrim1 l env Sub1   v = compilePrim2 l env Minus v (Number 1 l)
compilePrim1 l env IsNum  v = isType l env v TNumber
compilePrim1 l env IsBool v = isType l env v TBoolean
compilePrim1 _ env Print  v = call (builtin "print") [param env v]

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus    = arith     env addOp
compilePrim2 _ env Minus   = arith     env subOp
compilePrim2 _ env Times   = arith     env mulOp
compilePrim2 l env Less    = compare l env IJl (Just TNumber)
compilePrim2 l env Greater = compare l env IJg (Just TNumber)
compilePrim2 l env Equal   = compare l env IJe Nothing
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | immArg
--------------------------------------------------------------------------------
immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg _   (Boolean b _) = repr b
immArg env e@(Id x _)    = stackVar (fromMaybe err (lookupEnv x env))
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++ show (strip e)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Function call
--------------------------------------------------------------------------------
call :: Arg -> [Arg] -> [Instruction]
call f args
  =    ISub (Reg ESP) (Const (4 * k))
  :  [ IPush a | a <- reverse args ]
  ++ [ ICall f
     , IAdd (Reg ESP) (Const (4 * (n + k)))]
  where
    n = length args
    k = 4 - (n `mod` 4)


param :: Env -> IExp -> Arg
param env v = Sized DWordPtr (immArg env v)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

strip = fmap (const ())

-------------------------------------------------------------------------------
-- | TUPLES
-------------------------------------------------------------------------------
tupleAlloc :: Int -> [Instruction]
tupleAlloc n = [IMov (Reg EAX) (Reg ESI),
                IMov (Sized DWordPtr (RegOffset 0 EAX)) (repr n),
                IAdd (Reg ESI) (Const size)]
    where
        size = (4 * roundToEven (n + 1))

loadAddr :: Arg -> [Instruction]
loadAddr a = [IMov (Reg EAX) a,                      -- compute address
              ISub (Reg EAX) (Const 1),              -- drop tag bits
              IAnd (Reg EAX) (HexConst 0xfffffff8)]  -- set last 3 bits to 0

tupleRead :: Env -> IExp -> IExp -> [Instruction]
tupleRead env vE vI = assertType   env vE TTuple  ++
                      assertType   env vI TNumber ++
                      assertBound  env vE vI      ++
                      tupleReadRaw (immArg env vE) (immArg env vI)

tupleLoc :: Int -> Arg
tupleLoc i = RegOffset (4 * i) EAX

tupleReadRaw :: Arg -> Arg -> [Instruction]
tupleReadRaw aE aI =
     loadAddr aE	++
     [IMov (Reg EBX) aI,
      IShr (Reg EBX) (Const 1),
      IAdd (Reg EBX) (Const 1),
      IMov (Reg EAX) (RegIndex EAX EBX)]

tupleWrites :: [Arg] -> [Instruction]
tupleWrites args = concat (zipWith tupleWrite [1..] args)
    where
       tupleWrite i a = [IMov (Reg EBX) a,
                         IMov (Sized DWordPtr (tupleLoc i )) (Reg EBX)]

-- | Creates an (arity, label) pair instruction (Lambda Functions)
lamTuple :: Int -> Arity -> [Instruction]
lamTuple l arity = tupleAlloc 2                                  ++
                   tupleWrites [repr arity, CodePtr(LamStart l)] ++
                   [IOr (Reg EAX) (typeTag TClosure)]

-- | Creates an (arity, label) pair instruction (Named Functions)
funTuple :: Int -> Arity -> Label -> [Instruction]
funTuple l arity f = tupleAlloc 2                                    ++
                     tupleWrites [repr arity, CodePtr(DefStart f l)] ++
                     [IOr (Reg EAX) (typeTag TClosure)]

-- | Rounds up to next even
roundToEven :: Int -> Int
roundToEven n = if n `mod` 2 == 0 then n else (n + 1)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Arithmetic
--------------------------------------------------------------------------------
arith :: Env -> AOp -> IExp -> IExp  -> [Instruction]
arith env aop v1 v2
  =  assertType env v1 TNumber
  ++ assertType env v2 TNumber
  ++ IMov (Reg EAX) (immArg env v1)
   : IMov (Reg EBX) (immArg env v2)
   : aop (Reg EAX) (Reg EBX)


addOp :: AOp
addOp a1 a2 = [ IAdd a1 a2
              , overflow
              ]


subOp :: AOp
subOp a1 a2 = [ ISub a1 a2
              , overflow
              ]


mulOp :: AOp
mulOp a1 a2 = [ IMul a1 a2
              , overflow
              , ISar a1 (Const 1)
              ]


overflow :: Instruction
overflow = IJo (DynamicErr ArithOverflow)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Dynamic Tests
--------------------------------------------------------------------------------
-- | @isType 0@ tests if EAX is an Number,
--   @isType 1@ tests if EAX is a Boolean.
isType :: Tag -> Env -> IExp -> Ty -> [Instruction]
isType l env v ty
  =  cmpType env v ty
  ++ boolBranch  l IJe


-- | @assertType t@ tests if EAX is a value of type t and exits with error o.w.
assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty
  =   cmpType env v ty
  ++ [IJne (DynamicErr (TypeError ty))]



assertBound :: Env -> IExp -> IExp -> [Instruction]
assertBound env vE vI =
    [IMov (Reg EBX) (immArg env vI),
     ICmp (Reg EBX) (Sized DWordPtr (Const 0)),
     IJl  (DynamicErr IndexLow)]                ++
    loadAddr (immArg env vE)	                ++
    [IAdd (Reg EBX) (Const 2),
     ICmp (Reg EBX) (tupleLoc 0),
     IJg  (DynamicErr IndexHigh)]


assertArity :: Env -> Id -> Arity -> [Instruction]
assertArity env vE arity =
    [IMov (Reg EBX) (Const (envMax env)),
     ICmp (Reg EBX) arity,
     IJne (DynamicErr ArityError)]


cmpType :: Env -> IExp -> Ty -> [Instruction]
cmpType env v ty
  = [ IMov (Reg EAX) (immArg env v)
    , IMov (Reg EBX) (Reg EAX)
    , IAnd (Reg EBX) (typeMask ty)
    , ICmp (Reg EBX) (typeTag  ty)
    ]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Comparisons
--------------------------------------------------------------------------------
-- | @compare v1 v2@ generates the instructions at the
--   end of which EAX is TRUE/FALSE depending on the comparison
--------------------------------------------------------------------------------
compare :: Tag -> Env -> COp -> Maybe Ty -> IExp -> IExp -> [Instruction]
compare l env j t v1 v2
  =  compareCheck env t v1 v2
  ++ compareVal l env j v1 v2


compareCheck :: Env -> Maybe Ty -> IExp -> IExp -> [Instruction]
compareCheck _   Nothing  _  _
  =  []
compareCheck env (Just t) v1 v2
  =  assertType env v1 t
  ++ assertType env v2 t


compareVal :: Tag -> Env -> COp -> IExp -> IExp -> [Instruction]
compareVal l env j v1 v2
   = IMov (Reg EAX) (immArg env v1)
   : IMov (Reg EBX) (immArg env v2)
   : ICmp (Reg EAX) (Reg EBX)
   : boolBranch l j
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Assignment
--------------------------------------------------------------------------------
assign :: (Repr a) => Reg -> a -> Instruction
assign r v = IMov (Reg r) (repr v)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Branching
--------------------------------------------------------------------------------
branch :: Tag -> COp -> [Instruction] -> [Instruction] -> [Instruction]
branch l j falseIs trueIs = concat
  [ [ j lTrue ]
  , falseIs
  , [ IJmp lDone
    , ILabel lTrue  ]
  , trueIs
  , [ ILabel lDone ]
  ]
  where
    lTrue = (BranchTrue i)
    lDone = (BranchDone i)
    i     = snd l


boolBranch :: Tag -> COp -> [Instruction]
boolBranch l j = branch l j [assign EAX False] [assign EAX True]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


type AOp = Arg -> Arg -> [Instruction]
type COp = Label -> Instruction


stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP


--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------
class Repr a where
  repr :: a -> Arg


instance Repr Bool where
  repr True  = HexConst 0xffffffff
  repr False = HexConst 0x7fffffff


instance Repr Int where
  repr n = Const (fromIntegral (shift n 1))


instance Repr Integer where
  repr n = Const (fromIntegral (shift n 1))


typeTag :: Ty -> Arg
typeTag TNumber   = HexConst 0x00000000
typeTag TBoolean  = HexConst 0x7fffffff
typeTag TTuple    = HexConst 0x00000001
typeTag TClosure  = HexConst 0x00000005


typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
typeMask TTuple   = HexConst 0x00000007
typeMask TClosure = HexConst 0x00000007
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
