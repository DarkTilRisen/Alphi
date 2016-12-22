module Evaluator.StatementEval (evalStatement) where
import Data.Base
import Control.Monad.State
import Evaluator.Util
import Evaluator.BoolEval
import Evaluator.NumericEval


evalExpr :: Exp -> (StateT (Env ReturnValue) IO) ReturnValue
evalExpr (BExpr e)      = evalBoolExpr e
evalExpr (NExpr e)      = evalNumExpr  e
evalExpr (NAssign st e) = do {x <- evalNumExpr e ;state $ \s -> (x,insertVar st x s)};
evalExpr (BAssign st e) =  do {x <- evalBoolExpr e ;state $ \s -> (x,insertVar st x s)};


evalStatement :: Statement -> StateT (Env ReturnValue) IO ReturnValue
evalStatement (ExpStatement e)      = evalExpr e
evalStatement (Statements s1 s2)    = evalStatement s1 >> evalStatement s2
evalStatement (If b s)              = evalIf b s
evalStatement (While b s)           = evalWhile b s
evalStatement (Command Print e)     = evalPrintCommand e


evalIf :: Exp -> Statement ->  StateT (Env ReturnValue) IO ReturnValue
evalIf b s  = evalExpr b >>= check
  where check (Boolean True)  = evalStatement s >> return Void
        check (Boolean False) = return Void
        check x               = error impossibleState

evalWhile :: Exp -> Statement ->  StateT (Env ReturnValue) IO ReturnValue
evalWhile b s = evalExpr b >>= check
  where check (Boolean True)  = evalStatement s >> evalStatement (While b s)
        check (Boolean False) = return Void
        check x               = error impossibleState

evalPrintCommand :: Exp ->  StateT (Env ReturnValue) IO ReturnValue
evalPrintCommand e = do { x <- evalExpr e;
                          liftIO (print (getNum x));
                          return Void}
