module Evaluator.NumericEval where
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Parser.Base
import Data.Maybe
import Control.Monad.Trans.Maybe (MaybeT(..))
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.Util
import Parser.StatementParser
import Evaluator.Util

evalNumExpr :: NumericExp -> (StateT (Env2D) IO (Double))
evalNumExpr (LitDouble x)   = (return ) x
evalNumExpr (LitInteger x)  = (return . fromIntegral) x
evalNumExpr (BinaryNumericOp Add x y) = evalBOp (+) x y evalNumExpr
evalNumExpr (BinaryNumericOp Sub x y) = evalBOp (-) x y evalNumExpr
evalNumExpr (BinaryNumericOp Mul x y) = evalBOp (*) x y evalNumExpr
evalNumExpr (BinaryNumericOp Div x y) = evalBOp (/) x y evalNumExpr
evalNumExpr (NVar x)                  = getVar x snd
evalNumExpr (NAssign st x)            = assignSecond st x evalNumExpr
evalNumExpr _                         = error "ddddddd"
