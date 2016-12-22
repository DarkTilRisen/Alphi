module Evaluator.BoolEval where

import Control.Monad.State
import Data.Base
import Evaluator.Util
import Evaluator.NumericEval

evalBoolExpr :: BooleanExp -> (StateT (Env ReturnValue) IO (ReturnValue))
evalBoolExpr (LitBool x)                          = return (Boolean x)
evalBoolExpr (BinaryBoolOp And x y)               = evalBOp (&&) x y evalBoolExpr getBool Boolean
evalBoolExpr (BinaryBoolOp Or  x y)               = evalBOp (||) x y evalBoolExpr getBool Boolean
evalBoolExpr (BinaryAltBoolOp GreaterThan x y)    = evalBOp (>)  x y evalNumExpr getNum Boolean
evalBoolExpr (BinaryAltBoolOp SmallerThan x y)    = evalBOp (<)  x y evalNumExpr getNum Boolean
evalBoolExpr (BinaryAltBoolOp Equals x y )        = evalBOp (==) x y evalNumExpr getNum Boolean
evalBoolExpr (BVar x)                             = state $ \s -> (Boolean(getVar x s getBool),s)
