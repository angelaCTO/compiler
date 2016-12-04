{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Garter.Compiler ( compiler, compile, compileEnv, countVars, freeVars, typeTag, typeMask ) where

import           Prelude                  hiding (compare)
import           Control.Arrow            ((>>>))
import           Data.Maybe
import           Data.Bits                       (shift)
import qualified Data.Set                as S
-- import           Language.Garter.Utils
import           Language.Garter.Types      hiding (Tag)
import           Language.Garter.Parser     (parse)
import           Language.Garter.Checker    (check, errUnboundVar)
import           Language.Garter.Normalizer (anormal)
import           Language.Garter.Asm        (asm)

--------------------------------------------------------------------------------
-- | Compiler (Top Level)
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

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


bogusTag :: Tag
bogusTag = (mempty,0)


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
-- | FunInstr returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.
--------------------------------------------------------------------------------
funInstr :: Int -> [Instruction] -> [Instruction]
funInstr n instr = funEntry n ++ instr ++ funExit 

-- | Inserts instructions for setting up stack for n local variables
funEntry :: Int -> [Instruction]
funEntry n = [ IPush (Reg EBP)                       -- save caller's ebp
             , IMov  (Reg EBP) (Reg ESP)             -- set callee's ebp
             , ISub  (Reg ESP) (Const (4 * n))       -- allocate n local-vars
             , IAnd  (Reg ESP) (HexConst 0xFFFFFFF0)]-- MacOS stack alignment

-- | Cleans up stack and labels for jumping to error
funExit :: [Instruction]
funExit   = [ IMov (Reg ESP) (Reg EBP)              -- restore callee's esp
            , IPop (Reg EBP)                        -- restore callee's ebp
            , IRet]                                 -- jump back to caller
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | compile
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
compile _e = funInstr (countVars _e) (compileEnv emptyEnv _e)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | vars
--------------------------------------------------------------------------------
-- | countVars returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
countVars :: AnfExpr a -> Int
countVars (Let _ e b _)  = max (countVars e)  (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0


--FIXME ?
-- | freeVars takes in an `Expr a` and returns a list of free variables (no dups)
freeVars :: Expr a -> [Id]
freeVars e = S.toList(go e)
  where 
    go :: Expr a -> S.Set Id
    go (Id x _)           = S.singleton x
    go (Number _ _)       = S.empty
    go (Boolean _ _)      = S.empty
    go (If e e1 e2 _)     = S.unions [go e, go e1, go e2]
    go (App e es _)       = S.unions (map go (e:es))
    go (Let x e1 e2 _)    = S.union  (go e1) (S.delete (bindId x) (go e2))
    go (Lam xs e _)       = S.difference (go e) (S.fromList ((map bindId xs)))
    go (Fun x t xs e _)   = S.difference (go e) (S.fromList ((bindId x):(map bindId xs))) 
    go (Prim1 _ v _)      = go v
    go (Prim2 _ e1 e2 _ ) = S.unions [go e1, go e2]
    go (GetItem e f _)    = go e --FIXME ? 
    go (Tuple e1 e2 _ )   = S.unions [go e1, go e2] --FIXME ?
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | compileEnv
--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env v@(Number {})     = [ compileImm env v  ]
compileEnv env v@(Boolean {})    = [ compileImm env v  ]
compileEnv env v@(Id {})         = [ compileImm env v  ]

compileEnv env e@(Let {})        = is ++ compileEnv env' body
  where
    (env', is)                   = compileBinds env [] binds
    (binds, body)                = exprBinds e

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v
compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)    = IMov (Reg EAX) (immArg env v)
                                 : ICmp (Reg EAX) (repr False)
                                 : branch l IJe i1s i2s
  where
    i1s                          = compileEnv env e1
    i2s                          = compileEnv env e2

--error "TBD:compileEnv:tuple" FIXME ?
--Modify tuple implementation to work for pairs
compileEnv _env (Tuple _e1 _e2 _)    = tupleAlloc 2                  ++
                                       tupleWrites [immArg _env _e1] ++
                                       tupleWrites [immArg _env _e2] ++
                                       [IOr (Reg EAX) (typeTag TTuple)]

--error "TBD:compileEnv:getItem" FIXME ?
compileEnv _env (GetItem _vE _f _)   = loadAddr (immArg _env _vE) ++
                                       [IMov (Reg EBX) (getIndex _f),
                                        IShr (Reg EBX) (Const 1),
                                        IAdd (Reg EBX) (Const 1),
                                        IMov (Reg EAX) (RegIndex EAX EBX)]
                                                                                
--error "TBD:compileEnv:Lam"
compileEnv _env (Lam _xs _e _l)      = 
        IJmp   end                                          :               
        ILabel start                                        :                    
        lambdaBodyAnon ys xs' _e                            ++            
        ILabel end                                          :                      
        lamTuple bogusTag arity start _env ys      
    where
        ys    = freeVars (Lam _xs _e _l)
        arity = length _xs
        start = LamStart (snd _l)
        end   = LamEnd   (snd _l)
        xs'   = map bindId _xs

--error "TBD:compileEnv:Fun"
compileEnv _env (Fun _f _t _xs _e _l) = --Sig t not need here, but included to avoid complaint
        IJmp   end                                          :               
        ILabel start                                        :                    
        lambdaBody ys xs' _e                                ++            
        ILabel end                                          :                      
        lamTuple bogusTag arity start _env ys      
    where
        ys    = freeVars (Fun _f _t _xs _e _l)
        arity = length _xs
        start = LamStart (snd _l)
        end   = LamEnd   (snd _l)
        xs'   = map bindId (_f : _xs)

--error "TBD:compileEnv:App"
compileEnv _env (App _v _vs _l)      = 
        assertType _env _v TClosure                         ++
        assertArity _env _v (length _vs)                    ++
        tupleReadRaw (immArg _env _v) (repr (1 :: Int))     ++
        call (Reg EAX) (param _env <$> (_v : _vs))
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

getIndex :: Field -> Arg
getIndex Zero = (Const 0)
getIndex One  = (Const 1)


--------------------------------------------------------------------------------
-- | compileImm
--------------------------------------------------------------------------------
compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | compileBinds
--------------------------------------------------------------------------------
compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is ++ is') bs
  where
    (env', is')            = compileBind env b
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | compileBind
--------------------------------------------------------------------------------
compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      ++ [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | compilePrim(s)
--------------------------------------------------------------------------------
compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1   v = compilePrim2 l env Plus  v (Number 1 l)
compilePrim1 l env Sub1   v = compilePrim2 l env Minus v (Number 1 l)
compilePrim1 _ env Print  v = call (builtin "print") [param env v]

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus    = arith     env addOp
compilePrim2 _ env Minus   = arith     env subOp
compilePrim2 _ env Times   = arith     env mulOp
compilePrim2 l env Less    = compare l env IJl
compilePrim2 l env Greater = compare l env IJg
compilePrim2 l env Equal   = compare l env IJe
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

strip = fmap (const ())

--------------------------------------------------------------------------------
-- | Lamda (Anon Function)
--------------------------------------------------------------------------------
-- | lamTuple: creates a tuple of format (arity, label) 
lamTuple :: Tag -> Int -> Label -> Env -> [Id] -> [Instruction]
lamTuple bl arity start env ys =  
    lamAlloc arity (2 + length ys)                      ++  -- alloc tuple 2+|ys|  
    tupleWrites (--repr arity                           :   -- fill arity
                 CodePtr start                          :   -- fill code-ptr
                 [immArg env (Id y bogusTag) | y <- ys]
                )                                       ++  -- fill free-vars
    [IOr (Reg EAX) (typeTag TClosure)]                      -- set the tag bits   

-- | lamBody: restores free vares from closure-ptr then executes function body
lambdaBody :: [Id] -> [Id] -> AExp -> [Instruction]
lambdaBody ys xs e = funInstr maxStack (restore ys ++ compileEnv env e)
  where
    maxStack = envMax env + countVars e  -- max stack size
    env      = fromListEnv bs      
    bs       = zip xs  [-2,-3..] ++      --put params into env/stack
               zip ys  [1..]             --then, put free-vars into env/stack

-- | restores: variables onto the stack when function called (access values)
restore :: [Id] -> [Instruction]
restore ys  = concat [ copy i | (y, i) <- zip ys [1..]]
  where
    closPtr = RegOffset 8 EBP
    copy i  = tupleReadRaw closPtr (repr (i+1)) ++ --copy tuple-fld for y into EAX
              [IMov (stackVar i) (Reg EAX)]        --then, write EAX into stackVar for y

-- | lambdaBodyAnon
lambdaBodyAnon ys xs e = funInstr maxStack (restore ys ++ compileEnv env e)
  where
    maxStack = envMax env + countVars e  -- max stack size
    env      = fromListEnv bs      
    bs       = zip xs  [-3,-4..] ++      --put params into env/stack
               zip ys  [1..]             --then, put free-vars into env/stack
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | TUPLES
-------------------------------------------------------------------------------
-- | tupleWrites
tupleWrites :: [Arg] -> [Instruction]
tupleWrites args = concat (zipWith tupleWrite [1..] args)
    where
       tupleWrite i a = [IMov (Reg EBX) a,
                         IMov (Sized DWordPtr (tupleLoc i)) (Reg EBX)]

-- | tupleAlloc
tupleAlloc :: Int -> [Instruction]
tupleAlloc n = [IMov (Reg EAX) (Reg ESI),
                IMov (Sized DWordPtr (RegOffset 0 EAX)) (repr n),
                IAdd (Reg ESI) (Const size)]
    where
        size = (4 * roundToEven (n + 1))

-- | lamAlloc
lamAlloc :: Int -> Int -> [Instruction]
lamAlloc n p = [IMov (Reg EAX) (Reg ESI),
                IMov (Sized DWordPtr (RegOffset 0 EAX)) (repr n),
                IAdd (Reg ESI) (Const size)]
    where
        size = (4 * roundToEven (n + p + 1))

-- | loadAddr
loadAddr :: Arg -> [Instruction]
loadAddr a = [IMov (Reg EAX) a,                      -- compute address
              --ISub (Reg EAX) (Const 1),              -- drop tag bits
              IAnd (Reg EAX) (HexConst 0xfffffff8)]  -- set last 3 bits to 0

-- | tupleRead
tupleRead :: Env -> IExp -> IExp -> [Instruction]
tupleRead env vE vI = assertType   env vE TTuple  ++
                      assertType   env vI TNumber ++
                      assertBound  env vE vI      ++
                      tupleReadRaw (immArg env vE) (immArg env vI)

-- | tupleReadNoShift
tupleReadNoShift env vE vI = assertType   env vE TTuple  ++
                      assertType   env vI TNumber ++
                      assertBound  env vE vI      ++
                      tupleReadRawNoShift (immArg env vE) (immArg env vI)

-- | tupleLoc
tupleLoc :: Int -> Arg
tupleLoc i = RegOffset (4 * i) EAX

-- | tupleReadRaw
tupleReadRaw :: Arg -> Arg -> [Instruction]
tupleReadRaw aE aI =
     loadAddr aE	++
     [IMov (Reg EBX) aI,
      IShr (Reg EBX) (Const 1),
      --IAdd (Reg EBX) (Const 1),
      IMov (Reg EAX) (RegIndex EAX EBX)]

-- | tupleReadRawNoShift
tupleReadRawNoShift aE aI =
     loadAddr aE	++
     [IMov (Reg EBX) aI,
      IShr (Reg EBX) (Const 1),
      IAdd (Reg EBX) (Const 1),
      IMov (Reg EAX) (RegIndex EAX EBX)]

-- | roundToEven
roundToEven :: Int -> Int
roundToEven n = if n `mod` 2 == 0 then n else (n + 1)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Arithmetic
--------------------------------------------------------------------------------
arith :: Env -> AOp -> IExp -> IExp  -> [Instruction]
--------------------------------------------------------------------------------
arith env aop v1 v2
  =  IMov (Reg EAX) (immArg env v1)
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
mulOp a1 a2 = [ ISar a1 (Const 1)
              , IMul a1 a2
              , overflow
              ]

overflow :: Instruction
overflow = IJo (DynamicErr ArithOverflow)

--------------------------------------------------------------------------------
-- | Comparisons generates the instructions at the
--   end of which EAX is TRUE/FALSE depending on the comparison
--------------------------------------------------------------------------------
compare :: Tag -> Env -> COp -> IExp -> IExp -> [Instruction]
compare l env j v1 v2
   = IMov (Reg EAX) (immArg env v1)
   : IMov (Reg EBX) (immArg env v2)
   : ICmp (Reg EAX) (Reg EBX)
   : boolBranch l j
   
compareCheck :: Env -> Maybe Ty -> IExp -> IExp -> [Instruction]
compareCheck  _   Nothing  _  _  =  []
compareCheck env (Just t)  v1 v2 =  assertType env v1 t ++ assertType env v2 t

compareVal :: Tag -> Env -> COp -> IExp -> IExp -> [Instruction]
compareVal l env j v1 v2 = 
     IMov (Reg EAX) (immArg env v1)
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
-- | Function call
--------------------------------------------------------------------------------
-- | call
call :: Arg -> [Arg] -> [Instruction]
call f args
  =    ISub (Reg ESP) (Const (4 * k))
  :  [ IPush a | a <- reverse args ]
  ++ [ ICall f
     , IAdd (Reg ESP) (Const (4 * (n + k)))  ]
  where
    n = length args
    k = 4 - (n `mod` 4)

-- | param
param :: Env -> IExp -> Arg
param env v = Sized DWordPtr (immArg env v)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Branching
--------------------------------------------------------------------------------
-- | branch
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
    lTrue = BranchTrue i
    lDone = BranchDone i
    i     = snd l

-- | boolBranch
boolBranch :: Tag -> COp -> [Instruction]
boolBranch l j = branch l j [assign EAX False] [assign EAX True]

type AOp = Arg -> Arg -> [Instruction]
type COp = Label -> Instruction

-- | stackVar
stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Dynamic Tests
--------------------------------------------------------------------------------
-- | @isType 0@ tests if EAX is an Number,
--   @isType 1@ tests if EAX is a Boolean.
isType :: Tag -> Env -> IExp -> Ty -> [Instruction]
isType l env v ty =  cmpType env v ty ++ boolBranch  l IJe


-- | assertType: tests if EAX is a value of type t and exits with error o.w.
assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty
  =   cmpType env v ty
  ++ [IJne (DynamicErr (TypeError ty))]

-- | assertBound
assertBound :: Env -> IExp -> IExp -> [Instruction]
assertBound env vE vI =
    [IMov (Reg EBX) (immArg env vI),
     ICmp (Reg EBX) (Sized DWordPtr (Const 0)),
     IJl  (DynamicErr IndexLow)]                ++
    loadAddr (immArg env vE)	                ++
    [IAdd (Reg EBX) (Const 2),
     ICmp (Reg EBX) (tupleLoc 0),
     IJg  (DynamicErr IndexHigh)]

-- | assertArity
assertArity :: Env -> IExp -> Int -> [Instruction]    
assertArity env v arity =
    loadAddr (immArg env v) ++
    [IMov (Reg EBX) (RegOffset 0 EAX),
     ICmp (Reg EBX) (Const (arity*2)),
     IJne (DynamicErr ArityError)]

-- | cmpType:
cmpType :: Env -> IExp -> Ty -> [Instruction]
cmpType env v ty
  = [ IMov (Reg EAX) (immArg env v)
    , IMov (Reg EBX) (Reg EAX)
    , IAnd (Reg EBX) (typeMask ty)
    , ICmp (Reg EBX) (typeTag  ty)]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


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

instance Repr Field where
  repr Zero = repr (0 :: Int)
  repr One  = repr (1 :: Int)

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
