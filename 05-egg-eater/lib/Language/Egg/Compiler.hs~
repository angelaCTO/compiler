{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Egg.Compiler ( compiler, compile ) where

import           Prelude                  hiding (compare)
import           Control.Arrow                    ((>>>))
import           Data.Maybe
import           Data.Bits                       (shift)
import           Language.Egg.Utils
import           Language.Egg.Types      hiding (Tag)
import           Language.Egg.Parser     (parse)
import           Language.Egg.Checker    (check, errUnboundVar)
import           Language.Egg.Normalizer (anormal)
import           Language.Egg.Asm        (asm)


--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
-- compiler f = parse f >=> check >-> anormal >-> tag >-> compile >-> asm

compiler f = parse f >>> check >>> anormal >>> tag >>> compile >>> asm

--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag
type ADcl  = Decl    Tag
type APgm  = Program Tag

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: AnfProgram SourceSpan -> APgm
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
compile :: APgm -> [Instruction]
--------------------------------------------------------------------------------
compile (Prog ds e) = compileBody emptyEnv e
                   ++ concatMap compileDecl ds

compileDecl :: ADcl -> [Instruction]
compileDecl (Decl f xs e _) = ILabel (Builtin (bindId f))
                            : compileBody env e
  where
    env                     = fromListEnv (zip (bindId <$> xs) [-2, -3..])

compileBody :: Env -> AExp -> [Instruction]
compileBody env e = funInstrs (countVars e) (compileEnv env e)

-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.

funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs = funEntry n ++ instrs ++ funExit

-- FILL: insert instructions for setting up stack for `n` local vars
funEntry :: Int -> [Instruction]
funEntry n = [ IPush (Reg EBP)                       -- save caller's ebp
             , IMov  (Reg EBP) (Reg ESP)             -- set callee's ebp
             , ISub  (Reg ESP) (Const (4 * n))       -- allocate n local-vars
             , IAnd  (Reg ESP) (HexConst 0xFFFFFFF0) -- MacOS stack alignment
             ]

-- FILL: clean up stack & labels for jumping to error
funExit :: [Instruction]
funExit   = [ IMov (Reg ESP) (Reg EBP)          -- restore callee's esp
            , IPop (Reg EBP)                    -- restore callee's ebp
            , IRet                              -- jump back to caller
            ]

--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e b _)  = max (countVars e)  (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0


-------------------------------------------------------------------------------
-- | @compileEnv
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
compileEnv env (If v e1 e2 l)    = assertType env v TBoolean
                                ++ IMov (Reg EAX) (immArg env v)
                                 : ICmp (Reg EAX) (repr False)
                                 : branch l IJe i1s i2s
  where
    i1s                          = compileEnv env e1
    i2s                          = compileEnv env e2
compileEnv env (Tuple es _)      = tupleAlloc  (length es)         ++
                                   tupleWrites (immArg env <$> es) ++
				   [IOr (Reg EAX) (typeTag TTuple)]
{-
compileEnv env (Tuple es _)      = (allocHeap (length es))         ++
                                   (listCopy env (length es) es)   ++
				   [IMov (Reg EAX) (Reg EBX),
				    IAdd (Reg EAX) (HexConst 0x1)]
-}
compileEnv env (GetItem vE vI _) = tupleRead env vE vI
compileEnv env (App f vs _)      = call (Builtin f) (param env <$> vs)


-------------------------------------------------------------------------------
-- | @compileImm
-------------------------------------------------------------------------------
compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)


-------------------------------------------------------------------------------
-- | @compileBinds
-------------------------------------------------------------------------------
compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is ++ is') bs
  where
    (env', is')            = compileBind env b


-------------------------------------------------------------------------------
-- | @compileBind
-------------------------------------------------------------------------------
compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      ++ [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env


-------------------------------------------------------------------------------
-- | @compilePrim Unary Operations
-------------------------------------------------------------------------------
compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
--compilePrim1 l env Add1    v = compilePrim2 l env Plus  v (Number 1 l)
compilePrim1 l env Add1   v = (assertType TNumber 0 env v )         	++
                              [compileImm env v]                    	++
                              [IAdd (Reg EAX) (Const 2), 
                               IJo (DynamicErr (ArithOverflow))]

compilePrim1 l env Sub1    v = compilePrim2 l env Minus v (Number 1 l)
compilePrim1 l env IsNum   v = compileIs l env v 0
compileprim1 l env isBool  v = compileIs l env v 7
compilePrim1 l env IsTuple v = compileIs l env v 1
compilePrim1 _ env Print   v = call (Builtin "print") [param env v]


-------------------------------------------------------------------------------
-- | @compilePrim2 Binary Operators
-------------------------------------------------------------------------------
compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus    = arith     env addOp
compilePrim2 _ env Minus   = arith     env subOp
compilePrim2 _ env Times   = arith     env mulOp
compilePrim2 l env Less    = compare l env IJl (Just TNumber)
compilePrim2 l env Greater = compare l env IJg (Just TNumber)
compilePrim2 l env Equal   = compare l env IJe Nothing


-------------------------------------------------------------------------------
-- | @compileIS
-------------------------------------------------------------------------------
compileIs :: Tag -> Env -> IExp -> Int -> [Instruction]
compileIs l env v mask =  (compileEnv env v) ++
                          [IAnd (Reg EAX) (HexConst 7),
			   ICmp (Reg EAX) (HexConst mask),
			   IJe lTrue,
			   IMov (Reg EAX) (repr False),
			   IJmp lExit,
			   ILabel lTrue,
			   IMov (Reg EAX) (repr True),
			   ILabel lExit]
                      where 
			   lTrue = BranchTrue i
			   lExit = BranchDone i
			   l' = fst l
			   i = snd l


-------------------------------------------------------------------------------
-- | @immArg
-------------------------------------------------------------------------------
immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg _   (Boolean b _) = repr b
immArg env e@(Id x _)    = stackVar (fromMaybe err (lookupEnv x env))
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++show(strip e)


-------------------------------------------------------------------------------
-- | @strip
------------------------------------------------------------------------------
strip = fmap (const ())


--------------------------------------------------------------------------------
-- | Tuple Helper
-----------------
-- | @allocHeap
--------------------------------------------------------------------------------
allocHeap :: Int -> [Instruction]
allocHeap i = [IMov (Reg EBX) (Reg ESI),
               IMov (Reg EAX) (Const (shift i 1)),
               IMov (RegOffset 0 EBX) (Reg EAX),
               IAdd (Reg ESI) (Const (s*4)),
	       IAnd (Reg ESI) (HexConst 0xFFFFFFF8)]
	       where
	        s = i+s'
		s' = (i `mod` 2) 


-------------------------------------------------------------------------------
-- | @listCopy
-------------------------------------------------------------------------------
listCopy env l es = if l' == 0 then [] else
                    (compileEnv env h) ++
                    [IMov (RegOffset s EBX ) (Reg EAX)] ++
		     listCopy env l t
		     where
		       s = (1+l-l')*4
		       l' = length es
		       (h:t) = es


-------------------------------------------------------------------------------
-- | @tupleAlloc Tuple Manipulation
-------------------------------------------------------------------------------
tupleAlloc :: Int -> [Instruction]
tupleAlloc n = [IMov (Reg EAX) (Reg ESI),
                IMov (Sized DWordPtr (RegOffset 0 EAX)) (repr n),
                IAdd (Reg ESI) (Const size)
               ]
    where 
        size = (4 * roundToEven (n + 1))

-------------------------------------------------------------------------------
-- | @tupleReadRaw
-------------------------------------------------------------------------------
tupleReadRaw :: Arg -> Arg -> [Instruction]
tupleReadRaw aE aI = 
     loadAddr aE	++
     [IMov (Reg EBX) aI,
      IShr (Reg EBX) (Const 1),
      IAdd (Reg EBX) (Const 1),
      IMov (Reg EAX) (RegIndex EAX EBX)
     ]


-------------------------------------------------------------------------------
-- | @assertBound
-------------------------------------------------------------------------------
assertBound :: Env -> IExp -> IExp -> [Instruction]
assertBound env vE vI = 
    [IMov (Reg EBX) (immArg env vI),
     ICmp (Reg EBX) (Sized DWordPtr (Const 0)),
     IJl  (DynamicErr IndexLow)
    ]				++
    loadAddr (immArg env vE)	++
    [ICmp (Reg EBX) (tupleLoc 0),
     IJg  (DynamicErr IndexHigh)
    ]


-------------------------------------------------------------------------------
-- | @roundToEven  TODO
-------------------------------------------------------------------------------
roundToEven :: Int -> Int
roundToEven n = if n `mod` 2 == 0 then n else (n + 1)
  

-------------------------------------------------------------------------------
-- | @loadAddr env vE assigns to EAX the base address of tuple vE  TODO
-------------------------------------------------------------------------------
loadAddr :: Arg -> [Instruction]
loadAddr a = [IMov (Reg EAX) a,                    -- compute address
              ISub (Reg EAX) (typeTag ty),         -- drop tag bits
              IAnd (Reg EAX) (HexConst 0xfffffff8) -- set last 3 bits to 0
             ]



-------------------------------------------------------------------------------
-- | @tupleRead
-------------------------------------------------------------------------------
tupleRead :: Env -> IExp -> IExp -> [Instruction]
tupleRead env vE vI = assertType   env vE TTuple
                      assertType   env vI TNumber ++ 
                      assertBound  env vE vI      ++ 
                      tupleReadRaw (immArg env vE) (immArg vI)
 

-------------------------------------------------------------------------------
-- | @tupleWrites
-------------------------------------------------------------------------------
tupleWrites :: [Arg] -> [Instruction]
tupleWrites args = concat (zipWith [1..] args)
    where
        tupleWrites i a = [IMov (Reg EBX) a,
                           IMov (Sized DWordPtr (tupleLoc i )) (Reg EBX)
                          ]



-------------------------------------------------------------------------------
-- | @tupleLoc
-------------------------------------------------------------------------------
tupleLoc :: Int -> Arg
tupleLoc i = RegOffset (4 * i) EAX


-------------------------------------------------------------------------------
-- | Arithmetic
---------------
-- | @arith
--------------------------------------------------------------------------------
arith :: Env -> AOp -> IExp -> IExp  -> [Instruction]
--------------------------------------------------------------------------------
arith env aop v1 v2
  =  assertType env v1 TNumber
  ++ assertType env v2 TNumber
  ++ IMov (Reg EAX) (immArg env v1)
   : IMov (Reg EBX) (immArg env v2)
   : aop (Reg EAX) (Reg EBX)


-------------------------------------------------------------------------------
-- | @addOp
-------------------------------------------------------------------------------
addOp :: AOp
addOp a1 a2 = [ IAdd a1 a2
              , overflow
              ]

-------------------------------------------------------------------------------
-- | @subOp
-------------------------------------------------------------------------------
subOp :: AOp
subOp a1 a2 = [ ISub a1 a2
              , overflow
              ]


-------------------------------------------------------------------------------
-- | @milOp
-------------------------------------------------------------------------------
mulOp :: AOp
mulOp a1 a2 = [ IMul a1 a2
              , overflow
              , ISar a1 (Const 1)
              ]


-------------------------------------------------------------------------------
-- | @overflow
-------------------------------------------------------------------------------
overflow :: Instruction
overflow = IJo (DynamicErr ArithOverflow)


-------------------------------------------------------------------------------
-- | Dynamic Tests
------------------
-- | @assertType t@ tests if EAX is a value of type t and exits with error o.w.
-------------------------------------------------------------------------------
assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty
  =   cmpType env v ty
  ++ [ IJne (DynamicErr (TypeError ty))    ]


-------------------------------------------------------------------------------
-- | @cmpType
-------------------------------------------------------------------------------
cmpType :: Env -> IExp -> Ty -> [Instruction]
cmpType env v ty
  = [ IMov (Reg EAX) (immArg env v)
    , IMov (Reg EBX) (Reg EAX)
    , IAnd (Reg EBX) (typeMask ty)
    , ICmp (Reg EBX) (typeTag  ty)
    ]


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


-------------------------------------------------------------------------------
-- | @compareCheck
-------------------------------------------------------------------------------
compareCheck :: Env -> Maybe Ty -> IExp -> IExp -> [Instruction]
compareCheck _   Nothing  _  _
  =  []
compareCheck env (Just t) v1 v2
  =  assertType env v1 t
  ++ assertType env v2 t


-------------------------------------------------------------------------------
-- | @compareVal
-------------------------------------------------------------------------------
compareVal :: Tag -> Env -> COp -> IExp -> IExp -> [Instruction]
compareVal l env j v1 v2
   = IMov (Reg EAX) (immArg env v1)
   : IMov (Reg EBX) (immArg env v2)
   : ICmp (Reg EAX) (Reg EBX)
   : boolBranch l j


--------------------------------------------------------------------------------
-- | Assignment
---------------
-- | @assign
--------------------------------------------------------------------------------
assign :: (Repr a) => Reg -> a -> Instruction
assign r v = IMov (Reg r) (repr v)


--------------------------------------------------------------------------------
-- | Function call
------------------
-- | @call
--------------------------------------------------------------------------------
call :: Label -> [Arg] -> [Instruction]
call f args
  =    ISub (Reg ESP) (Const (4 * k))
  :  [ IPush a | a <- reverse args ]
  ++ [ ICall f
     , IAdd (Reg ESP) (Const (4 * (n + k)))  ]
  where
    n = length args
    k = 4 - (n `mod` 4)


-------------------------------------------------------------------------------
-- | @param
-------------------------------------------------------------------------------
param :: Env -> IExp -> Arg
param env v = Sized DWordPtr (immArg env v)


--------------------------------------------------------------------------------
-- | Branching
---------------
-- | @branch
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
    lTrue = BranchTrue i
    lDone = BranchDone i
    i     = snd l


-------------------------------------------------------------------------------
-- | @boolBranch
-------------------------------------------------------------------------------
boolBranch :: Tag -> COp -> [Instruction]
boolBranch l j = branch l j [assign EAX False] [assign EAX True]


-------------------------------------------------------------------------------
-- | @stackVar
-------------------------------------------------------------------------------
stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP


type AOp = Arg -> Arg -> [Instruction]
type COp = Label -> Instruction


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

typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
typeMask TTuple   = HexConst 0x00000007
