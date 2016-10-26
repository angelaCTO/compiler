{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Cobra.Compiler ( compiler, compile ) where

import           Control.Monad                   (void)
import           Control.Arrow                   ((>>>))
import           Text.Printf                     (printf)
import           Prelude                  hiding (compare)
import           Data.Maybe
import           Data.Bits                       (shift)
import           Language.Cobra.Types      hiding (Tag)
import           Language.Cobra.Parser     (parse)
import           Language.Cobra.Normalizer (anormal)
import           Language.Cobra.Asm        (asm)


--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> anormal >>> tag >>> compile >>> asm

--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag

instance Located Tag where
  sourceSpan = fst

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: Bare -> AExp
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = funInstrs (countVars e) (compileEnv emptyEnv e)

-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.
funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs = funEntry n ++ instrs ++ funExit

--TODO | TBD:insert instructions for setting up stack-frame for `n` local vars
funEntry :: Int -> [Instruction]
funEntry n  = [ IPush (Reg EBP), IMov (Reg EBP) ( Reg ESP), ISub (Reg ESP) (Const (4*n))]

--TODO | TBD: cleaning up stack-frame after function finishes
funExit :: [Instruction]
funExit   = [ IMov ( Reg ESP ) ( Reg EBP ), IPop ( Reg EBP), IRet]

--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e b _)  = max (countVars e)  (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@Number {}       = [ compileImm env v  ]

compileEnv env v@Boolean {}      = [ compileImm env v  ]

compileEnv env v@Id {}           = [ compileImm env v  ]

compileEnv env e@Let {}          = is ++ compileEnv env' body
  where
    (env', is)                   = compileBinds env [] binds
    (binds, body)                = exprBinds e

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)    = compileIf l env v e1 e2

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

--TODO | TBD: Implement code for `Prim1` with appropriate type checking
{-
-- | `Prim1` are unary operations
data Prim1
  = Add1
  | Sub1
  | Print
  | IsNum
  | IsBool
  deriving (Show)
-}

compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
--compilePrim1 l env op v = error "TBD:compilePrim1"
compilePrim1 l env op v = case (l, env, op, v) of
    (l, env, Add1, v)   ->( assertType TNumber 0 env v )++[compileImm env v]++
                         [ IAdd (Reg EAX) (Const 2) ]
    (l, env, Sub1, v)   -> (assertType TNumber 0 env v) ++[compileImm env v]++
                         [ ISub (Reg EAX) (Const 2) ]
    (l, env, Print, v)  -> [compileImm env v]++
                         [ IPush (Reg EAX),
			   ICall (Builtin "print"),
			   IAdd (Reg ESP) (Const 4)
			 ]
    (l, env, IsNum, v)  -> compileIs l env v 0 

    (l, env, IsBool, v) -> compileIs l env v 1

compileIs l env v mask =  (compileEnv env v) ++
                         [ IAnd (Reg EAX) (HexConst 1),
			   ICmp (Reg EAX) (Const mask),
			   IJe lTrue,
			   IMov (Reg EAX) (repr False),
			   IJmp lExit,
			   ILabel lTrue,
			   IMov (Reg EAX) (repr True),
			   ILabel lExit
			 ]
			 where 
			   lTrue = BranchTrue i
			   lExit = BranchDone i
			   i = snd l

--TODO | TBD: Implement code for `Prim2` with appropriate type checking
{-
-- | `Prim2` are binary operations
data Prim2
  = Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
  deriving (Show)
-}
compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
--compilePrim2 l env op = error "TBD:compilePrim2"
compilePrim2 l env op v1 v2 = case (l, env, op, v1, v2) of
    (l, env, Plus,  v1, v2)    -> assertType TNumber 0 env v1 ++ assertType TNumber 0 env v2 ++
                                  [IMov (Reg EAX) (immArg env v1),
				  IAdd (Reg EAX) (immArg env v2),
                                  IJo (DynamicErr (ArithOverflow))
			          ]
    (l, env, Minus, v1, v2)    ->  assertType TNumber 0 env v1 ++ assertType TNumber 0 env v2 ++
                                  [IMov (Reg EAX) (immArg env v1),
				   ISub (Reg EAX) (immArg env v2),
                                   IJo (DynamicErr (ArithOverflow))
				  ]

    (l, env, Times, v1, v2)    ->  assertType TNumber 0 env v1 ++ assertType TNumber 0 env v2 ++
                                  [IMov (Reg EAX) (immArg env v1),
				  ISar (Reg EAX) (Const 1),
                                  IMul (Reg EAX) (immArg env v2),
				  IJo (DynamicErr (ArithOverflow))
			          ]

 
    (l, env, Less,    v1, v2) -> assertType TNumber 0 env v1 ++ assertType TNumber 0 env v2 ++
                                 compileCmp l env IJl v1 v2
    (l, env, Greater, v1, v2) -> assertType TNumber 0 env v1 ++ assertType TNumber 0 env v2 ++
                                 compileCmp l env IJg v1 v2
    (l, env, Equal, v1, v2)   -> assertType TNumber 0 env v1 ++ assertType TNumber 0 env v2 ++
                                 compileCmp l env IJe  v1 v2

assertType::Ty->Int->Env->IExp->[Instruction]
assertType ty i env v = [ IMov (Reg EAX) (immArg env v),
                       IAnd (Reg EAX) (Const 1),
		       ICmp (Reg EAX) (Const i),
                       IJne (DynamicErr (TypeError ty))
	             ]

--compileCmp :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compileCmp l env op v1 v2 =
              [ IMov (Reg EAX) (immArg env v1),
	        ICmp (Reg EAX) (immArg env v2),
		op lTrue,
		IMov (Reg EAX) (repr False),
		IJmp lExit,
		ILabel lTrue,
		IMov (Reg EAX) (repr True),
		ILabel lExit
	      ]
	      where
	        lTrue = BranchTrue i
		lExit = BranchDone i
		i = snd l

--TODO | TBD: Implement code for `If` with appropriate type checking
compileIf :: Tag -> Env -> IExp -> AExp -> AExp -> [Instruction]
compileIf l env v e1 e2    =
    assertType TBoolean 1 env v ++ [compileImm env v]
    ++ [ICmp (Reg EAX) (repr False),
        IJne (BranchTrue (snd l))  --l=label (fst l =sourcespan, snd l =tag)
       ]
    ++ compileEnv env e2
    ++ [IJmp (BranchDone (snd l)),
        ILabel (BranchTrue (snd l))
       ]
    ++ compileEnv env e1
    ++ [ILabel (BranchDone (snd l))]



immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg _   (Boolean b _) = repr b
immArg env e@(Id x _)    = stackVar (fromMaybe err (lookupEnv x env))
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++ show (void e)

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l

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

typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
