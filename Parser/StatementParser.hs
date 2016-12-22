module Parser.StatementParser (parseStatement) where
import Control.Applicative
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base
import Parser.BoolParser

parseExp :: Parser Exp
parseExp =        assign' parseBoolExp BAssign bool
          `mplus` assign' parseNumberExp NAssign num
          `mplus` fmap BExpr parseBoolExp
          `mplus` fmap NExpr parseNumberExp
        where assign' e c t = do {matchStr t; s <- parseAlpha ; matchStr assign;x <- e; return (c s x) }

matchEnd :: Parser a -> Parser a
matchEnd p = do {x <- p; matchStr stop; return x}

parseStatementExp :: Parser Statement
parseStatementExp = matchEnd $  ExpStatement <$> parseExp

parseWhile :: Parser Statement
parseWhile = parseStruct while  While

parseIf :: Parser Statement
parseIf =  parseStruct if' If

parseStruct :: String -> (Exp -> Statement -> Statement) -> Parser Statement
parseStruct s c = do { matchStr s;x <- parseExp ; y <- parseBrackets parseStatement ; return $ c x y}

parseCommand :: String -> Command -> Parser Statement
parseCommand s c = matchEnd $ fmap (Command c) (matchStr command >> matchStr s >> parseExp)
parseStatement :: Parser Statement
parseStatement = base `chainl1` return Statements
        where base = parseStatementExp
                    `mplus` parseWhile
                    `mplus` parseIf
                    `mplus` parseCommand print' Print
