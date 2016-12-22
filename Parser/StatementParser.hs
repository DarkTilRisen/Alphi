module Parser.StatementParser where
import Control.Applicative (Alternative(..))
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base
import Parser.BoolParser

parseExp :: Parser Exp
parseExp = (assign' parseBoolExp BAssign bool)
          `mplus` (assign' parseNumberExp NAssign num)
          `mplus` fmap BExpr parseBoolExp
          `mplus` fmap NExpr parseNumberExp
        where assign' e c t = do {matchStr t; s <- parseAlpha ; matchStr assign;x <- e; return (c s x) }

matchEnd :: Parser a -> Parser a
matchEnd p = do {x <- p; matchStr stop; return x}

parseStatementExp :: Parser Statement
parseStatementExp = matchEnd $ fmap ExpStatement $ parseExp

parseWhile :: Parser Statement
parseWhile = do {matchStr while;
                 x <- parseExp;
                 y <- parseBrackets parseStatement;
                 return (While x y)}

parseIf :: Parser Statement
parseIf = do {matchStr if';
              x <- parseExp;
              y <- parseBrackets parseStatement;
              return $ If x y}

--parseCommand :: String -> Parser Statement
{-parseCommand s c = do {matchStr command;
                    matchStr s;
                    x <- parseExp;
                    return (Command c x);
                  }

-}
parseStatement :: Parser Statement
parseStatement = base `chainl1` (return Statements)
        where base = parseStatementExp `mplus` parseWhile `mplus` parseIf
