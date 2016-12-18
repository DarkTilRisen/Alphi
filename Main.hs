module Main where
import Parser.Base
import Parser.NumericParser

main =  do { x <- readFile "voorbeeld.txt";
              putStrLn (show $ parse parseNumberExp x);
            }
