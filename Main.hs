module Main where
import Parser.Base
import Parser.NumericParser
import Parser.BoolParser
import Data.Base

main =  print $  parse (parseNumberExp) "444ADD 444";
