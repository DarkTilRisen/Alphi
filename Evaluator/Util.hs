module Evaluator.Util where
import Control.Monad.State
import Data.Base
import Data.Maybe
import System.HIDAPI hiding (error)

-- evaluate 2 expressions apply a function and return a MyState with the evaluated expression as result
evalBOp :: Monad m => (t3 -> t3 -> t2) -> t -> t -> (t -> m t1) -> (t1 -> t3) -> (t2 -> b) -> m b
evalBOp f x y e g c = do x' <- e x
                         y' <- e y
                         return (c (f (g x') (g y')))

-- find a value in a environment
find ::(Eq a) => a -> [(a,b)] -> b
find x env = fromMaybe (error varNotFound) (lookup x env)

-- insert a value
insertVar :: String -> a -> Env a -> Env a
insertVar s a env =  (s,a):remove s env
  where remove s  = filter (\(s1,a) -> (s1 /= s))


-- Insert a return value
insert :: String -> ReturnValue ->  MyState
insert st x = state $ \s -> (x, insertVar st x s)

-- Get a variable
getVar :: String -> Env a -> (a -> b) -> b
getVar s env f = f $ find s env

-- Return the device from the state
returnDevice :: MyState
returnDevice =  state $ \s -> (find device s, s)


getDevice :: ReturnValue -> Device
getDevice (Dev d) = d
getDevice x       = error impossibleState

-- Extract a double from a return value
getNum :: ReturnValue -> Double
getNum (Num x)    = x
getNum x          = error impossibleState

-- Extract a boolean from a return value
getBool :: ReturnValue -> Bool
getBool (Boolean x) = x
getBool x           = error impossibleState
