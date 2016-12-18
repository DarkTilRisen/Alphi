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
parseInt = do { x <- parseDigits;
                return $ (read x :: Int)}

--parses an Float
parseDouble :: Parser Double
parseDouble = do { x <- parseDigits; parseString floatSep;
                   y <- parseDigits ;
                   return $ (read (x ++ "." ++ y) :: Double)}

--parse an LiteralNumber
parseNumLiteral :: Parser NumericExp
parseNumLiteral = parseLit LitInteger parseInt <|> parseLit LitDouble parseDouble
                  where parseLit cons f = (<|>) (f >>= return . cons) (token '-' >>  f >>= return . cons . (0-))-


                     --

-- easyfy order of expressions
chainExp:: Parser NumericExp -> [(String, NumericBinaryOp)] -> Parser NumericExp
chainExp acc xs = chainl1 acc $ foldl1 mplus $ fmap f xs
                    where f (s, cons) = createLit' s BinaryNumericOp cons--matchStr s >> (return . BinaryNumericOp) cons


parseNumberAbstractExp :: Parser NumericExp -> [[(String, NumericBinaryOp)]] -> Parser NumericExp
parseNumberAbstractExp = foldl chainExp

parseNumberExp :: Parser NumericExp
parseNumberExp = parseNumberAbstractExp base orderBNumOp
                    where base =  parseNumLiteral <|> parseParens parseNumberExp
