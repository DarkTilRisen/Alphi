module Parser.NumericParser (parseNumberExp) where
import Control.Applicative
import Control.Monad
import Parser.Base
import Data.Char
import Parser.Util
import Data.Base

-- Parser for a series of digits
parseDigits :: Parser String
parseDigits = plus $ spot isDigit

-- Parser for an integer
parseInt :: Parser Int
parseInt = fmap read parseDigits

-- Parser for doubles
parseDouble :: Parser Double
parseDouble = do  x <- parseDigits;
                  parseString floatSep;
                  y <- parseDigits;
                  return (read (x ++ "." ++ y) :: Double)

-- Parser for an numerical variable
parseNumVar :: Parser NumericExp
parseNumVar = token num >> fmap NVar parseAlpha

-- Parser for an numeral
parseNumLiteral :: Parser NumericExp
parseNumLiteral = parseTrailingSpace $ fmap LitInteger parseInt
                               `mplus` fmap LitDouble parseDouble


-- Function to make order of expressions easier
chainExp:: Parser NumericExp -> [(String, NumericBinaryOp)] -> Parser NumericExp
chainExp acc xs = chainl1 acc $ parseFromTuple' f xs
                    where f (s, cons) = createP1' s BinaryNumericOp cons

-- Parser for all numerical expressions
parseNumberExp :: Parser NumericExp
parseNumberExp = foldl chainExp base orderBNumOp
                    where base =        parseNumVar
                                `mplus` parseNumLiteral
                                `mplus` parseParens parseNumberExp
