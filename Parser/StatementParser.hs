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

parseAssign :: Parser Statement
parseAssign = do { x <-  parseAlpha;
                   matchStr assign;
                   y <- parseExp;
                   matchStr stop;
                   return $ Var x y }

parseIf :: Parser Statement
parseIf = do {matchStr while;
              x <- parseBoolExp;
              y <- parseBrackets parseStatement;
              return $ If x y}

parseStatements :: Parser Statement
parseStatements = do { x <- parseStatement; y <- parseStatement; return (Statements x y) }


parseStatement :: Parser Statement
parseStatement = parseWhile `mplus` parseAssign `mplus` parseIf `mplus` parseStatements
