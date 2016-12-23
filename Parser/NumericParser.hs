module Parser.NumericParser (parseNumberExp) where
import Control.Applicative
import Control.Monad
import Parser.Base
import Data.Char
import Parser.Util
import Data.Base

--parses a series of digits
parseDigits :: Parser String
parseDigits = plus $ spot isDigit

--parses an Int
parseInt :: Parser Int
parseInt = fmap read parseDigits

--parses a Float
parseDouble :: Parser Double
parseDouble = do { x <- parseDigits;
                   parseString floatSep;
                   y <- parseDigits;
                   return (read (x ++ "." ++ y) :: Double)}

parseNumVar :: Parser NumericExp
parseNumVar = token num >> fmap NVar parseAlpha

--parse an LiteralNumber
parseNumLiteral :: Parser NumericExp
parseNumLiteral = parseTrailingSpace $ fmap LitInteger parseInt `mplus` fmap LitDouble parseDouble


-- easyfy order of expressions
chainExp:: Parser NumericExp -> [(String, NumericBinaryOp)] -> Parser NumericExp
chainExp acc xs = chainl1 acc $ parseFromTuple' f xs
                    where f (s, cons) = createP1' s BinaryNumericOp cons

parseNumberExp :: Parser NumericExp
parseNumberExp = foldl chainExp base orderBNumOp
                    where base =        parseNumVar
                                `mplus` parseNumLiteral 
                                `mplus` parseParens parseNumberExp
