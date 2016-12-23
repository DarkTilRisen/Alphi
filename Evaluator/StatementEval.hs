module Evaluator.StatementEval (evalStatement) where
import Data.Base
import Control.Monad.State
import Evaluator.Util
import Evaluator.BoolEval
import Evaluator.NumericEval
import Robot.Base
import MBot hiding (Command)

evalExp :: Exp -> (StateT (Env ReturnValue) IO) ReturnValue
evalExp (BExp e)       = evalBoolExp e
evalExp (NExp e)       = evalNumExp  e
--evalExp (NAssign st e) = do {x <- evalNumExpr e  ;state $ \s -> (x,insertVar st x s)};
--evalExp (BAssign st e) = do {x <- evalBoolExpr e ;state $ \s -> (x,insertVar st x s)};


evalStatement :: Statement -> StateT (Env ReturnValue) IO ReturnValue
evalStatement (ExpStatement e)      = evalExp e
evalStatement (Statements s1 s2)    = evalStatement s1 >> evalStatement s2
evalStatement (If b s)              = evalIf b s
evalStatement (While b s)           = evalWhile b s
evalStatement (Assign s e)          = evalAssign s e
evalStatement (Output t e)         = evalCommand t e


evalCommand :: Command -> Exp -> StateT (Env ReturnValue) IO ReturnValue
evalCommand Print  e  = evalExp e >>= (liftIO . print . getNum) >> return Void
evalCommand MotorRight e  = do {x <- evalExp e;
                                liftIO (openMBot >>= move ((floor . getNum) x) MotorR ) >> return Void
                                }

evalIf :: Exp -> Statement ->  StateT (Env ReturnValue) IO ReturnValue
evalIf b s  = evalExp b >>= check
  where check (Boolean True)  = evalStatement s >> return Void
        check (Boolean False) = return Void
        check x               = error impossibleState

evalWhile :: Exp -> Statement ->  StateT (Env ReturnValue) IO ReturnValue
evalWhile b s = evalExp b >>= check
  where check (Boolean True)  = evalStatement s >> evalStatement (While b s)
        check (Boolean False) = return Void
        check x               = error impossibleState

evalAssign :: String -> Exp -> StateT (Env ReturnValue) IO ReturnValue
evalAssign st (BExp e) = do {x <- evalBoolExp e  ;state $ \s -> (x,insertVar st x s)};
evalAssign st (NExp e) = do {x <- evalNumExp  e  ;state $ \s -> (x,insertVar st x s)}
