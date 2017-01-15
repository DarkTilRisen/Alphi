module Evaluator.BoolEval (evalBoolExp) where

import Control.Monad.State
import Data.Base
import Evaluator.Util
import Evaluator.NumericEval


-- Evaluate boolean expressions
-- Uses evalNumExp for binary operators this will evaluate both expressions
-- apply the given function and return a value wrapped in a MyState
evalBoolExp :: BooleanExp -> MyState
evalBoolExp (LitBool x)                          = return (Boolean x)
evalBoolExp (UnaryBoolOp Not x)                  = fmap (Boolean . not . getBool) (evalBoolExp x)
evalBoolExp (BinaryBoolOp And x y)               = evalBOp (&&) x y evalBoolExp getBool Boolean
evalBoolExp (BinaryBoolOp Or  x y)               = evalBOp (||) x y evalBoolExp getBool Boolean
evalBoolExp (BinaryAltBoolOp GreaterThan x y)    = evalBOp (>)  x y evalNumExp getNum Boolean
evalBoolExp (BinaryAltBoolOp SmallerThan x y)    = evalBOp (<)  x y evalNumExp getNum Boolean
evalBoolExp (BinaryAltBoolOp Equals x y )        = evalBOp (==) x y evalNumExp getNum Boolean
evalBoolExp (BVar x)                             = state $ \s -> (find x s, s)
