module Main where

import Data.Char
import Data.Base
import Parser.Base
import Control.Monad.State
import Parser.StatementParser
import Evaluator.StatementEval
import Parser.Util
import Control.Applicative
import System.Environment


-- parses a string to a Statement
parseAll :: String -> Statement
parseAll = parseResult finalParse
        where finalParse = parseLeadingSpace >> parseStatement

-- parses and evaluates a given string
eval :: String -> IO (ReturnValue, Env ReturnValue)
eval = flip (runStateT . evalStatement . parseAll) emptyEnv

main :: IO (ReturnValue, Env ReturnValue)
main = getArgs >>= readFile . head >>= eval
