module Main where

import Data.Char
import Data.Base
import Parser.Base
import Control.Monad.State
import Parser.StatementParser
import Evaluator.StatementEval
import Parser.Util
import Control.Applicative


-- parses a string to a Statement
parseAll :: String -> Statement
parseAll = parseResult finalParse
        where finalParse = parseLeadingSpace >> parseStatement

-- parses and evaluates a given string
eval :: String -> IO (ReturnValue, Env ReturnValue)
eval = flip (runStateT . evalStatement . parseAll) emptyEnv

--run the parser and evaluator from a given file.
main :: IO ()
main = readFile "AlphiExamples/demo_line_smooth.alp" >>= eval >> print "done"
