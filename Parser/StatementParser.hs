module Parser.StatementParser where
import Control.Applicative (Alternative(..))
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base
import Parser.BoolParser

parseExp :: Parser Statement
parseExp =  fmap ExpStatement $ matchEnd $ fmap BExpr parseBoolExp `mplus` fmap NExpr parseNumberExp
              where matchEnd p = do {x <- p; matchStr stop; return x}

parseWhile :: Parser Statement
parseWhile = do {matchStr while;
                 x <- parseBoolExp;
                 y <- parseBrackets parseStatement;
                 return $ While x y}

parseIf :: Parser Statement
parseIf = do {matchStr if';
              x <- parseBoolExp;
              y <- parseBrackets parseStatement;
              return $ If x y}

parseStatement :: Parser Statement
parseStatement =  parseExp `mplus` parseWhile `mplus` parseIf `chainl1` (return Statements)
