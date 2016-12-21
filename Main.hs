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

--main =  print $ parseResult (parseStatement) "WHILE TRUE BEGIN tobiah IS 4 STOP END t IS 4 STOP t IS 5 STOP t IS 6 STOP"
main = do {x <- readFile "AlphiExamples/demo1.alp";
           putStrLn $ show $  (parseResult (parseStatement) x);
           y <- (runStateT (evalStatement (parseResult parseStatement x)) ([], []));
           putStrLn $ show $  y ;}
