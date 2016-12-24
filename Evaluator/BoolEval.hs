module Evaluator.BoolEval (evalBoolExp) where

import Control.Monad.State
import Data.Base
import Evaluator.Util
import Evaluator.NumericEval

evalBoolExp :: BooleanExp -> MyState
evalBoolExp (LitBool x)                          = return (Boolean x)
evalBoolExp (UnaryBoolOp Not x)                  = fmap (Boolean . not . getBool) (evalBoolExp x)
evalBoolExp (BinaryBoolOp And x y)               = evalBOp (&&) x y evalBoolExp getBool Boolean
evalBoolExp (BinaryBoolOp Or  x y)               = evalBOp (||) x y evalBoolExp getBool Boolean
evalBoolExp (BinaryAltBoolOp GreaterThan x y)    = evalBOp (>)  x y evalNumExp getNum Boolean
evalBoolExp (BinaryAltBoolOp SmallerThan x y)    = evalBOp (<)  x y evalNumExp getNum Boolean
evalBoolExp (BinaryAltBoolOp Equals x y )        = evalBOp (==) x y evalNumExp getNum Boolean
evalBoolExp (BVar x)                             = state $ \s -> (Boolean(getVar x s getBool),s)
