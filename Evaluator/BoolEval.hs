module Evaluator.BoolEval where

import Control.Monad.State
import Data.Base
import Evaluator.Util
import Evaluator.NumericEval
evalBoolExpr :: BooleanExpr -> (StateT (Env2D) IO (Bool))
evalBoolExpr (LitBool x)                       = return  x
evalBoolExpr (UnaryBoolOp Not x )              = evalUOp (not) x evalBoolExpr
evalBoolExpr (BinaryBoolOp And x y)            = evalBOp (&&) x y evalBoolExpr
evalBoolExpr (BinaryBoolOp Or  x y)            = evalBOp (||) x y evalBoolExpr
evalBoolExpr (BinaryAltBoolOp GreaterThan x y) = evalBOp (>)  x y evalNumExpr
evalBoolExpr (BinaryAltBoolOp SmallerThan x y) = evalBOp (<)  x y evalNumExpr
evalBoolExpr (BinaryAltBoolOp Equals x y )     = evalBOp (==) x y evalNumExpr
evalBoolExpr (BVar x)                          = getVar x fst
evalBoolExpr (BAssign st x)                    = assignFirst st x evalBoolExpr
