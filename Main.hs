module Main where
import Control.Applicative (Alternative(..))
import Control.Monad
import Parser.Base
import Parser.NumericParser
import Parser.BoolParser
import Data.Base

main =  print $ parse (parseNumberExp) "1 ADD 1"
