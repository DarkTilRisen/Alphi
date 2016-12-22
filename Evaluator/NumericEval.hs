module Evaluator.NumericEval (evalNumExpr) where
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Parser.Base
import Control.Monad.Trans.Maybe (MaybeT(..))
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.Util
import Parser.StatementParser
import Evaluator.Util

evalNumExpr :: NumericExp -> StateT (Env ReturnValue) IO ReturnValue
evalNumExpr (LitDouble x)             = (return . Num ) x
evalNumExpr (LitInteger x)            = (return . Num . fromIntegral) x
evalNumExpr (BinaryNumericOp Add x y) = evalBOp (+) x y evalNumExpr getNum Num
evalNumExpr (BinaryNumericOp Sub x y) = evalBOp (-) x y evalNumExpr getNum Num
evalNumExpr (BinaryNumericOp Mul x y) = evalBOp (*) x y evalNumExpr getNum Num
evalNumExpr (BinaryNumericOp Div x y) = evalBOp (/) x y evalNumExpr getNum Num
evalNumExpr (NVar x)                  = state $ \s -> (Num (getVar x s getNum),s)
