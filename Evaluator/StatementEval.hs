module Evaluator.StatementEval where
import Data.Base
import Control.Monad.State
import Evaluator.Util
import Evaluator.BoolEval
import Evaluator.NumericEval


evalExpr :: Exp -> (StateT (Env2D) IO ())
evalExpr (BExpr e) = evalBoolExpr e >> return ()
evalExpr (NExpr e) = evalNumExpr  e >> return ()

evalStatement :: Statement -> StateT Env2D (IO) ()
evalStatement (ExpStatement e)   = evalExpr e >> return ()
evalStatement (Statements s1 s2) = evalStatement s1 >> evalStatement s2 >> return ()
evalStatement (If b s)           = evalIf b s
evalStatement (While b s)        = evalWhile b s

evalIf :: BooleanExpr -> Statement ->  StateT Env2D (IO) ()
evalIf b s  = evalBoolExpr b >>= check
  where check True  = evalStatement s >> return ()
        check (False) = return ()

evalWhile :: BooleanExpr -> Statement ->  StateT Env2D (IO) ()
evalWhile b s = evalBoolExpr b >>=  \x -> check x
  where check True  = evalStatement s >> evalStatement (While b s)
        check False = return ()
