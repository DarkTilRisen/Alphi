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

-- evaluate boolean expressions
-- Uses evalNumExp for binary operators this will evaluate both expressions
-- apply the given function and return a value wrapped in a MyState
evalNumExp :: NumericExp -> MyState
evalNumExp (LitDouble x)             = (return . Num ) x
evalNumExp (LitInteger x)            = (return . Num . fromIntegral) x
evalNumExp (BinaryNumericOp Add x y) = evalBOp (+)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Sub x y) = evalBOp (-)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Mul x y) = evalBOp (*)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Div x y) = evalBOp (/)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Mod x y) = evalBOp mod'' x y evalNumExp getNum Num
evalNumExp (NVar x)                  = state $ \s -> (find x s,s)

-- modulo for doubles what a stupid hack
mod'' :: Double -> Double -> Double
mod'' x y  = fromIntegral $ mod (round x ) (round y)
