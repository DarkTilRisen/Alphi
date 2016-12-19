module Parser.NumericParser (parseNumberExp) where
import Control.Applicative (Alternative(..))
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

--parses an Float
parseDouble :: Parser Double
parseDouble = do { x <- parseDigits;
                   parseString floatSep;
                   y <- parseDigits;
                   return $ (read (x ++ "." ++ y) :: Double)}

--parse an LiteralNumber
parseNumLiteral :: Parser NumericExp
parseNumLiteral = parseTrailingSpace $ fmap LitInteger parseInt <|> fmap LitDouble parseDouble


-- easyfy order of expressions
chainExp:: Parser NumericExp -> [(String, NumericBinaryOp)] -> Parser NumericExp
chainExp acc xs = chainl1 acc $ parseFromTuple' f xs
                    where f (s, cons) = createP1' s BinaryNumericOp cons

parseNumberExp :: Parser NumericExp
parseNumberExp = foldl chainExp base orderBNumOp
                    where base =  parseNumLiteral <|> parseParens parseNumberExp
