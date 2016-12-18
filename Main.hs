module Main where
import Control.Applicative (Alternative(..))
import Control.Monad
import Base
import Data
import NumericParsers
import Util

main =  print $ parse (parseNumberExp) "he"
