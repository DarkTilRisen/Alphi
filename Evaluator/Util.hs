module Evaluator.Util where
import Control.Monad.State
import Data.Base


evalBOp :: Monad m => (t1 -> t1 -> b) -> t -> t -> (t -> m t1) -> m b
evalBOp f x y e = do {x' <- e x; y' <- e y; return (f x' y')}


evalUOp :: Monad m => (t -> b) -> t1 -> (t1 -> m t) -> m b
evalUOp f x e   = do {x' <- e x; return (f x')}

--assign' :: String -> a -> (a -> (State (Env b) (b))) -> (Env2D -> Env b) -> (State (Env2D) (b))
--assignfirst st x e f = e x >>= \y -> state $ \s -> getEnv x
--
--assignSecond :: String -> a -> (a -> b) -> (State (Env2D) (b))
--assignSecond :: Monad State ([(t2, b)], t1) m => t2 -> t -> (t -> m b) -> m b
assignSecond :: String -> NumericExp ->  (NumericExp -> (State (Env2D) (Double))) -> (State (Env2D) (Double))
assignSecond st x e = e x >>= \y -> state $ \s -> (y,(setEnv s st y))
  where setEnv (b, d) st x = (b,(st, x):d)

assignFirst :: String -> BooleanExpr ->  (BooleanExpr -> (State (Env2D) (Bool))) -> (State (Env2D) (Bool))
assignFirst st x e = e x >>= \y -> state $ \s -> (y,(setEnv s st y))
  where setEnv (b, d) st x = ((st, x):b,d)

lookup' x env = f $ lookup x env
  where f (Just x)  = x
        f (Nothing) = error "something went wrong"

getVar ::String -> (Env2D -> Env b) -> (State (Env2D) (b))
getVar x f = state $ \s -> (lookup' x (f s),s)
