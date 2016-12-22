module Evaluator.Util where
import Control.Monad.State
import Data.Base
import Data.Maybe

evalBOp :: Monad m => (t3 -> t3 -> t2) -> t -> t -> (t -> m t1) -> (t1 -> t3) -> (t2 -> b) -> m b
evalBOp f x y e g c = do {x' <- e x; y' <- e y; return (c (f (g x') (g y')))}

-- find a value in an environment
lookup' :: Eq a => a -> [(a, b)] -> b
lookup' x env =  fromMaybe (error varNotFound) (lookup x env)

insertVar :: String -> a -> Env a -> Env a
insertVar s a env =  (s,a):remove s env
  where remove s = filter (\(s1,a) -> (s1 /= s))

-- extract a value from an environment
getVar :: String -> Env a -> (a -> b) -> b
getVar s env f = f (lookup' s env)

-- extract a Double from a return  value
getNum :: ReturnValue -> Double
getNum (Num x) = x
getNum x       = error impossibleState

-- extract boolean from  retrun value
getBool :: ReturnValue -> Bool
getBool (Boolean x) = x
getBool x           = error impossibleState
