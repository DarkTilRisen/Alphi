module Evaluator.StatementEval (evalStatement) where
import Data.Base
import Control.Monad.State
import Evaluator.Util
import Evaluator.BoolEval
import Evaluator.NumericEval
import Robot.Base
import System.HIDAPI (Device)
import MBot hiding (Command)


-- Evaluate expressions
evalExp :: Exp -> MyState
evalExp (BExp  e) = evalBoolExp e
evalExp (NExp  e) = evalNumExp  e
evalExp (Input c) = evalInput   c

-- Evaluate input commands
evalInput :: INCommand -> MyState
evalInput ReadUltra          = evalInput' readUltra Num
evalInput LineLeft           = evalInput' (readLine SensorL) Boolean
evalInput LineRight          = evalInput' (readLine SensorR) Boolean
evalInput OpenBotConnection  = fmap Dev (liftIO openMBot) >>= insert device
evalInput CloseBotConnection = returnDevice >>= liftIO . closeMBot . getDevice >> return Void

-- Evaluate statements
evalStatement :: Statement -> MyState
evalStatement (ExpStatement e)   = evalExp e
evalStatement (Statements s1 s2) = evalStatement s1 >> evalStatement s2
evalStatement (If b s)           = evalStruct b s $ evalStatement s
evalStatement (While b s)        = evalStruct b s $ evalStatement s >> evalStatement (While b s)--evalStruct b s $ evalStatement s >> evalStatement (While b s)
evalStatement (Assign s e)       = evalAssign s e
evalStatement (Output t e)       = evalCommand t e
evalStatement Empty              = return Void

-- Evaluate output commands
evalCommand :: OUTCommand -> Exp -> MyState
evalCommand Print (BExp e)       = evalPrint (evalBoolExp e) getBool
evalCommand Print (NExp e)       = evalPrint (evalNumExp e)  getNum
evalCommand MotorRight e         = evalRobotFunction e MotorR move
evalCommand MotorLeft  e         = evalRobotFunction e MotorL move
evalCommand Data.Base.Led1 e     = evalRobotFunction e Robot.Base.Led1 led
evalCommand Data.Base.Led2 e     = evalRobotFunction e Robot.Base.Led2 led


-- Evaluate input
evalInput' :: (Device -> IO a) -> (a -> ReturnValue) -> MyState
evalInput' f c = fmap c (returnDevice >>= liftIO . f . getDevice)
-- Evaluate print commands
evalPrint :: (Show a) => MyState -> (ReturnValue -> a) -> MyState
evalPrint e f = e >>= (liftIO . print . f) >> return Void

-- evaluates a robot instruction
-- e is the expression
-- f is a function that returns an IO
evalRobotFunction :: Integral c => Exp -> t -> (c -> t -> Device -> IO a) -> StateT (Env ReturnValue) IO ReturnValue
evalRobotFunction e l f = do x <- evalExp e
                             d <- returnDevice;
                             liftIO (f ((floor . getNum) x) l (getDevice d))
                             return Void

-- Evaluate structures like while and if
evalStruct :: Exp -> Statement -> MyState -> MyState
evalStruct b s f = evalExp b >>= check
  where check (Boolean True)  = f
        check (Boolean False) = return Void
        check x               = error impossibleState

-- Evaluate assignment
evalAssign :: String -> Exp -> MyState
evalAssign st e = evalExp e >>= insert st
