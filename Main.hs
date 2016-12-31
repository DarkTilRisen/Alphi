module Main where

import Data.Char
import Data.Base
import Parser.Base
import Control.Monad.State
import Parser.StatementParser
import Evaluator.StatementEval


-- parses a string to a Statement
parseAll :: String -> Statement
parseAll = parseResult parseStatement . dropWhile isSpace

-- parses and evaluates a given string
eval :: String -> IO (ReturnValue, Env ReturnValue)
eval = flip (runStateT . evalStatement . parseAll) emptyEnv

--run the parser and evaluator from a given file.
main :: IO ()
main = do {st <- readFile "AlphiExamples/demo_police.alp";
--           (print . parseAll) st;
           eval st;
           print "done!!!!!";
           print $ show (round 4.0)
          }
