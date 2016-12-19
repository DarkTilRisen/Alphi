module Main where
import Control.Applicative (Alternative(..))
import Control.Monad
import Parser.Base
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.StatementParser

--main =  print $ parse (parseStatement) "WHILE TRUE BEGIN tobiah IS 4 STOP END t IS 4 STOP t IS 5 STOP t IS 6 STOP"
