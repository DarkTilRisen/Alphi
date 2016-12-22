module Parser.StatementParser where
import Control.Applicative (Alternative(..))
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base
import Parser.BoolParser

parseExp :: Parser Exp
parseExp =         (assign' parseBoolExp BAssign bool)
          `mplus` (assign' parseNumberExp NAssign num)
          `mplus` fmap BExpr parseBoolExp
          `mplus` fmap NExpr parseNumberExp
        where assign' e c t = do {matchStr t; s <- parseAlpha ; matchStr assign;x <- e; return (c s x) }

matchEnd :: Parser a -> Parser a
matchEnd p = do {x <- p; matchStr stop; return x}

parseStatementExp :: Parser Statement
parseStatementExp = matchEnd $ fmap ExpStatement $ parseExp

parseWhile :: Parser Statement
parseWhile = parseStruct while parseExp (parseBrackets parseStatement) While

parseIf :: Parser Statement
parseIf =  parseStruct if' parseExp (parseBrackets parseStatement) If

parseStruct :: String -> Parser a -> Parser b -> (a-> b -> c) -> Parser c
parseStruct s e e1 c = do { matchStr s;x <- e ; y <- e1; return $ c x y}

parseCommand :: String -> Command -> Parser Statement
parseCommand s c = matchEnd (matchStr command >> matchStr s >> parseExp >>=(return .(Command c)))

parseStatement :: Parser Statement
parseStatement = base `chainl1` (return Statements)
        where base = parseStatementExp
                    `mplus` parseWhile
                    `mplus` parseIf
                    `mplus` (parseCommand print' Print)
