module Parser.StatementParser where
import Control.Applicative (Alternative(..))
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base
import Parser.BoolParser

parseExp :: Parser Exp
parseExp = fmap BExpr parseBoolExp `mplus` fmap NExpr parseNumberExp

parseWhile :: Parser Statement
parseWhile = do {matchStr while;
                 x <- parseBoolExp;
                 y <- parseBrackets parseStatement;
                 return $ While x y}

parseIf :: Parser Statement
parseIf = undefined 

parseStatement :: Parser Statement
parseStatement = undefined
