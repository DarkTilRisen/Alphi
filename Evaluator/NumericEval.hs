module Evaluator.NumericEval (evalNumExp) where
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Parser.Base
import Control.Monad.Trans.Maybe
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.Util
import Parser.StatementParser
import Evaluator.Util

evalNumExp :: NumericExp -> MyState
evalNumExp (LitDouble x)             = (return . Num ) x
evalNumExp (LitInteger x)            = (return . Num . fromIntegral) x
evalNumExp (BinaryNumericOp Add x y) = evalBOp (+) x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Sub x y) = evalBOp (-) x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Mul x y) = evalBOp (*) x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Div x y) = evalBOp (/) x y evalNumExp getNum Num
--evalNumExp (BinaryNumericOp Mod x y) = evalBOp Mod x y evalNumExp getNum Num
evalNumExp (NVar x)                  = state $ \s -> (Num (getVar x s getNum),s)
