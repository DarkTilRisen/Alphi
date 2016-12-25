module Evaluator.StatementEval (evalStatement) where
import Data.Base
import Control.Monad.State
import Evaluator.Util
import Evaluator.BoolEval
import Evaluator.NumericEval
import Robot.Base
import System.HIDAPI (Device)
import MBot hiding (Command)


--Evaluate expressions
evalExp :: Exp -> MyState
evalExp (BExp  e) = evalBoolExp e
evalExp (NExp  e) = evalNumExp  e
evalExp (Input c) = evalInput   c

-- Evaluate input commands
evalInput :: INCommand -> MyState
evalInput ReadUltra          = evalInput' readUltra Num
evalInput LineLeft           = evalInput' (readLine SensorL) Boolean
evalInput LineRight          = evalInput' (readLine SensorR) Boolean
evalInput OpenBotConnection  = liftIO openMBot >>= return . Dev >>= insert device
evalInput CloseBotConnection = returnDevice >>= liftIO . closeMBot . getDevice >> return Void

-- Evaluate statements
evalStatement :: Statement -> MyState
evalStatement (ExpStatement e)   = evalExp e
evalStatement (Statements s1 s2) = evalStatement s1 >> evalStatement s2
evalStatement (If b s)           = evalStruct b s $ evalStatement s
evalStatement (While b s)        = evalStruct b s $ evalStatement s >> evalStatement (While b s)
evalStatement (Assign s e)       = evalAssign s e
evalStatement (Output t e)       = evalCommand t e
evalStatement Empty              = return Void

-- Evaluate output commands
evalCommand :: OUTCommand -> Exp -> MyState
evalCommand Print (BExp e)       = evalPrint (evalBoolExp e) getBool
evalCommand Print (NExp e)       = evalPrint (evalNumExp e)  getNum
evalCommand MotorRight e         = evalMotor e MotorR
evalCommand MotorLeft  e         = evalMotor e MotorL

-- Evaluate input
evalInput' :: (Device -> IO a) -> (a -> ReturnValue) -> MyState
evalInput' f c = returnDevice >>= liftIO . f . getDevice >>= (return . c)

-- Evaluate print commands
evalPrint :: (Show a) => MyState -> (ReturnValue -> a) -> MyState
evalPrint e f = e >>= (liftIO . print . f) >> return Void

-- evaluate motor commands
evalMotor :: Exp -> Motor ->  MyState
evalMotor e m = do { x <- evalExp e; d <- returnDevice;liftIO (move ((floor .getNum) x) m (getDevice d)); return Void }

-- Evaluate structures like while and if
evalStruct :: Exp -> Statement -> MyState -> MyState
evalStruct b s f = evalExp b >>= check
  where check (Boolean True)  = f
        check (Boolean False) = return Void
        check x               = error impossibleState

-- Evaluate assignment
evalAssign :: String -> Exp -> MyState
evalAssign st e = evalExp e >>= insert st
