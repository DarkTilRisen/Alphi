module Main where
import Control.Applicative (Alternative(..))
import Control.Monad.State
import Control.Monad
import Parser.Base
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.Util
import Evaluator.StatementEval
import Parser.StatementParser

main = do {x <- readFile "AlphiExamples/demo2.alp";
           putStrLn $ show $  (parse (parseStatement) x);
            y <- (runStateT  (evalStatement (parseResult parseStatement x)) emptyEnv2D);
           putStrLn $ show $  y;;}
