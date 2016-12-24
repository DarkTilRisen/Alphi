module Main where
import System.HIDAPI hiding (error)
import Control.Applicative
import Control.Monad.State
import Control.Monad
import Parser.Base
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.Util
import Evaluator.StatementEval
import Data.Char
import MBot
import Robot.Base
import Parser.StatementParser

parseAll :: String -> Statement
parseAll s = parseResult parseStatement (dropWhile isSpace s)

eval :: String -> Device -> IO (ReturnValue, Env ReturnValue)
eval s d = runStateT  (evalStatement (parseAll s)) ([], d)

main :: IO ()
main = do {st <- readFile "AlphiExamples/demo_line.alp";
           print $ parse parseStatement (dropWhile isSpace st);
           d  <- openMBot;
           eval st d;
           closeMBot d; }
