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
evalNumExp (LitDouble x)             = (return . Num ) x -- Just return the number wrapped in MyState
evalNumExp (LitInteger x)            = (return . Num . fromIntegral) x -- Convert to a number and wrap it in a Mystate
evalNumExp (BinaryNumericOp Add x y) = evalBOp (+)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Sub x y) = evalBOp (-)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Mul x y) = evalBOp (*)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Div x y) = evalBOp (/)  x y evalNumExp getNum Num
evalNumExp (BinaryNumericOp Mod x y) = evalBOp mod'' x y evalNumExp getNum Num
evalNumExp (NVar x)                  = state $ \s -> (find x s,s) --lookup the value in the environment and return it


-- modulo for doubles what a stupid hack
mod'' :: Double -> Double -> Double
mod'' x y  = fromIntegral $ mod (round x ) (round y)
