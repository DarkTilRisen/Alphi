module Main where
import Control.Applicative (Alternative(..))
import Control.Monad
import Parser.Base
import Parser.NumericParser
import Parser.BoolParser
import Data.Base
import Parser.StatementParser

main =  print $ parse (parseAlpha) "WHILE TRUE BEGIN 1ADD2MULOPEN3MUL4CLOSEGT4ANDTRUE END"
