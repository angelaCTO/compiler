{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Diamondback.Compiler ( compiler, compile ) where

import           Data.Monoid
import           Control.Arrow                    ((>>>))
import           Prelude                  hiding (compare)
import           Control.Monad                   (void)
import           Data.Maybe
import           Data.Bits                       (shift)
import           Language.Diamondback.Types      hiding (Tag)
import           Language.Diamondback.Parser     (parse)
import           Language.Diamondback.Checker    (check, errUnboundVar)
import           Language.Diamondback.Normalizer (anormal)
import           Language.Diamondback.Label
import           Language.Diamondback.Asm        (asm)


--------------------------------------------------------------------------------
-- | @compiler
--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
compiler f = parse f >>> check >>> anormal >>> tag >>> tails >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Ann@
--------------------------------------------------------------------------------
type Ann   = ((SourceSpan, Int), Bool)
type AExp  = AnfExpr Ann
type IExp  = ImmExpr Ann
type ABind = Bind    Ann
type ADcl  = Decl    Ann
type APgm  = Program Ann

instance Located Ann where
  sourceSpan = fst . fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel


-------------------------------------------------------------------------------
-- | @annTag
-------------------------------------------------------------------------------
annTag :: Ann -> Int
annTag = snd . fst


-------------------------------------------------------------------------------
-- | @annTail
-------------------------------------------------------------------------------
annTail :: Ann -> Bool
annTail = snd


--------------------------------------------------------------------------------
-- | @compile 							
--------------------------------------------------------------------------------
compile :: APgm -> [Instruction]
compile (Prog ds e) = compileBody emptyEnv e <> concatMap compileDecl ds


-------------------------------------------------------------------------------
-- | @compileDecl 							
-------------------------------------------------------------------------------
compileDecl :: ADcl -> [Instruction]
compileDecl (Decl f xs e l) = ILabel (DefFun (bindId f)) : compileBody env e
    where 
      env = fromListEnv (zip (bindId <$> xs) [-2, -3..])


-------------------------------------------------------------------------------
-- | @compileBody
-------------------------------------------------------------------------------
compileBody :: Env -> AExp -> [Instruction]
compileBody env e = funInstrs (countVars e) (compileEnv env e)


-------------------------------------------------------------------------------
-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.		
-------------------------------------------------------------------------------
funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs = funEntry n ++ 
                     instrs     ++ 
                     funExit    ++ 
                     [IRet]


-------------------------------------------------------------------------------
-- | @funEntry sets up stack for `n` local vars 			
------------------------------------------------------------------------------
funEntry :: Int -> [Instruction]
funEntry n  = [ IPush (Reg EBP), 
                IMov  (Reg EBP) (Reg ESP), 
                ISub  (Reg ESP) (Const (4 * n)) 
              ]


-------------------------------------------------------------------------------
-- | @funExit cleans up stack & labels for jumping to error 		
-------------------------------------------------------------------------------
funExit :: [Instruction]
funExit = [ IMov (Reg ESP) (Reg EBP), 
            IPop (Reg EBP) 
          ]


--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e 
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
countVars (Let _ e b _)  = max (countVars e) (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0


-------------------------------------------------------------------------------
-- | @compileEnv 						(TODO - CHECK)
-------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env v@Number  {} 		= [ compileImm env v ]
compileEnv env v@Boolean {} 		= [ compileImm env v ]
compileEnv env v@Id      {} 		= [ compileImm env v ]
compileEnv env e@Let     {} 		= is ++ compileEnv env' body
    where  
        (env', is)     = compileBinds env [] binds
        (binds, body)  = exprBinds e
compileEnv env (Prim1 o v l)		= compilePrim1 l env o v
compileEnv env (Prim2 o v1 v2 l)	= compilePrim2 l env o v1 v2
compileEnv env (If v e1 e2 l)		= compileIf l env v e1 e2
compileEnv env (App f vs l)
    | annTail l		= tailcall (DefFun f) (param env <$> vs) 
    | otherwise		= call     (DefFun f) (param env <$> vs) 


-------------------------------------------------------------------------------
-- | @compilePrim1
-------------------------------------------------------------------------------
compilePrim1 :: Ann -> Env -> Prim1 -> IExp -> [Instruction]  
compilePrim1 _ env Add1   v = (assertType TNumber 0 env v )         	++
                              [compileImm env v]                    	++
                              [IAdd (Reg EAX) (Const 2), 
                               IJo (DynamicErr (ArithOverflow))]
compilePrim1 _ env Sub1   v = (assertType TNumber 0 env v)     		++
                              [compileImm env v]                    	++
                              [ISub (Reg EAX) (Const 2),
                               IJo (DynamicErr (ArithOverflow))]
compilePrim1 l env IsNum  v = compileIs l env v 0 
compilePrim1 l env IsBool v = compileIs l env v 1
compilePrim1 _ env Print  v = call (Builtin "print") [param env v]


-------------------------------------------------------------------------------
-- | @compilePrim2
-------------------------------------------------------------------------------
compilePrim2 :: Ann -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus    v1 v2 = (assertType TNumber 0 env v1)      	++ 
                                   (assertType TNumber 0 env v2)      	++
                                   [IMov (Reg EAX) (immArg env v1),
				    IAdd (Reg EAX) (immArg env v2),
                                    IJo (DynamicErr (ArithOverflow))]

compilePrim2 _ env Minus   v1 v2 = (assertType TNumber 0 env v1)      	++ 
                                   (assertType TNumber 0 env v2)      	++
                                   [IMov (Reg EAX) (immArg env v1),
				    ISub (Reg EAX) (immArg env v2),
                                    IJo (DynamicErr (ArithOverflow))]

compilePrim2 _ env Times   v1 v2 = (assertType TNumber 0 env v1)      	++ 
                                   (assertType TNumber 0 env v2)      	++
                                   [IMov (Reg EAX) (immArg env v1),
				    ISar (Reg EAX) (Const 1),
                                    IMul (Reg EAX) (immArg env v2),
				    IJo (DynamicErr (ArithOverflow))]

compilePrim2  l env Less    v1 v2 = (assertType TNumber 0 env v1)       ++ 
                                   (assertType TNumber 0 env v2)	++
                                   compileCmp a env IJl v1 v2
			  where
			  	(a, _) = l
compilePrim2  l env Greater v1 v2 = (assertType TNumber 0 env v1) 	++ 
                                   (assertType TNumber 0 env v2) 	++
                                    compileCmp a env IJg v1 v2
			  where
			  	(a, _) = l
compilePrim2  l env Equal   v1 v2 = (assertType TNumber 0 env v1) 	++ 
                                   (assertType TNumber 0 env v2) 	++
                                   compileCmp a env IJe  v1 v2
			  where
			  	(a, _) = l
				(_, f) = a
-------------------------------------------------------------------------------
-- | @compileCmp
-------------------------------------------------------------------------------
--compileCmp :: Ann -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compileCmp l env op v1 v2 =
              [IMov (Reg EAX) (immArg env v1),
	       ICmp (Reg EAX) (immArg env v2),
	       op lTrue,
	       IMov (Reg EAX) (repr False),
	       IJmp lExit,
	       ILabel lTrue,
	       IMov (Reg EAX) (repr True),
	       ILabel lExit]
	  where
	        lTrue = BranchTrue i
		lExit = BranchDone i
		i = snd l


------------------------------------------------------------------------------
-- | @compileIf
------------------------------------------------------------------------------
compileIf :: Ann -> Env -> IExp -> AExp -> AExp -> [Instruction]
compileIf l env v e1 e2 = (assertType TBoolean 1 env v) ++ 
                          [compileImm env v]		++
			  [ICmp (Reg EAX) (repr False),
                           IJne (BranchTrue (f))]	++ --l=label (fst l =sourcespan, snd l =tag)
			  (compileEnv env e2)		++ 
			  [IJmp (BranchDone (f)),
                           ILabel (BranchTrue (f))]	++ 
			  (compileEnv env e1)		++ 
			  [ILabel (BranchDone (f))]
			  where
			  	(a, _) = l
				(_, f) = a


------------------------------------------------------------------------------
-- | @compileIs
------------------------------------------------------------------------------
compileIs :: Ann -> Env -> IExp -> Int -> [Instruction]
compileIs l env v mask =  (compileEnv env v) ++
                          [IAnd (Reg EAX) (HexConst 1),
			   ICmp (Reg EAX) (Const mask),
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
			   i = snd l'


------------------------------------------------------------------------------
-- | @assertType
------------------------------------------------------------------------------
assertType::Ty->Int->Env->IExp->[Instruction]
assertType ty i env v = [IMov (Reg EAX) (immArg env v),
                         IAnd (Reg EAX) (Const 1),
		         ICmp (Reg EAX) (Const i),
                         IJne (DynamicErr (TypeError ty))]


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
compileBinds env is (b:bs) = compileBinds env' (is <> is') bs
    where
        (env', is') = compileBind env b


-------------------------------------------------------------------------------
-- | @compileBind
-------------------------------------------------------------------------------
compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
    where
        is        = compileEnv env e <> [IMov (stackVar i) (Reg EAX)]
        (i, env') = pushEnv x env

                           
-------------------------------------------------------------------------------
-- | @immArg
-------------------------------------------------------------------------------
immArg :: Env -> IExp -> Arg
immArg _      (Number  n _)  = repr n
immArg _      (Boolean b _)  = repr b
immArg env  e@(Id      x _)  = stackVar (fromMaybe err (lookupEnv x env))
    where 
        err = abort (errUnboundVar (sourceSpan e) x)
immArg _    e 	             = panic msg (sourceSpan e)
    where
        msg = "Unexpected non-immExpr in immArg: " <> show (void e)


-------------------------------------------------------------------------------
-- | @stackVar
-------------------------------------------------------------------------------
stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP



-------------------------------------------------------------------------------
-- | @call 								
-------------------------------------------------------------------------------
call :: Label -> [Arg] -> [Instruction]
call f args = [IPush a | a <- reverse args]     ++
              [ICall f]                         ++
              [IAdd (Reg ESP) (Const (4 * n))]  
    where 
        n = length args


-------------------------------------------------------------------------------
-- | @param
-------------------------------------------------------------------------------
param :: Env -> IExp -> Arg
param env v = Sized DWordPtr (immArg env v)


--------------------------------------------------------------------------------
-- | @tailcall 							(TODO - CHECK)
--------------------------------------------------------------------------------
tailcall :: Label -> [Arg] -> [Instruction]
tailcall f args = copyArgs 1 args [] ++
                  [IMov (Reg ESP) (Reg EBP),
		   IPop (Reg EBP),
		   IJmp f]

copyArgs n args acc = if t == 0 then acc else
                      copyArgs n' args' acc' 
		        where
			  t = length args
			  n' = n+1
			  (h:args') = args
			  acc' = [IMov (Reg EAX) h,
			          IMov (RegOffset ((n+1)*4) EBP) (Reg EAX)]++acc

-------------------------------------------------------------------------------
-- | @assign
-------------------------------------------------------------------------------
assign :: (Repr a) => Reg -> a -> Instruction
assign r v = IMov (Reg r) (repr v)


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


-------------------------------------------------------------------------------
-- | @typeTag
-------------------------------------------------------------------------------
typeTag :: Ty -> Arg
typeTag TNumber   = HexConst 0x00000000
typeTag TBoolean  = HexConst 0x7fffffff


-------------------------------------------------------------------------------
-- | @typeMask
-------------------------------------------------------------------------------
typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
