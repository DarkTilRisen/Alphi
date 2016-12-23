module Main where
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
import System.HIDAPI hiding (error)
import Parser.StatementParser

parseAll :: String -> Statement
parseAll s = parseResult parseStatement (dropWhile isSpace s)

eval :: String -> Device -> IO (ReturnValue, Env ReturnValue)
eval s d = runStateT  (evalStatement (parseAll s)) ([], d)

main :: IO ()
main = do {st <- readFile "AlphiExamples/demo-1.alp";
           d  <- openMBot;
           x  <- eval st d;
           --print x;
           closeMBot d; }--readFile "AlphiExamples/demo-1.alp" >>= eval >>= print
