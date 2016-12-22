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
import Parser.StatementParser

parseAll :: String -> Statement
parseAll s = parseResult parseStatement (dropWhile isSpace s)

eval :: String -> IO (ReturnValue, Env ReturnValue)
eval s = runStateT  (evalStatement (parseAll s)) []

main :: IO ()
main = readFile "AlphiExamples/demo-1.alp" >>= eval >>= print
