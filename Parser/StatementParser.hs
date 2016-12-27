module Parser.StatementParser (parseStatement) where
import Control.Applicative
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base
import Parser.BoolParser

-- Parser for an Expression
parseExp :: Parser Exp
parseExp =        fmap BExp parseBoolExp
          `mplus` fmap NExp parseNumberExp
          `mplus` parseINCommand sensorL  LineLeft
          `mplus` parseINCommand sensorR  LineRight
          `mplus` parseINCommand ultra    ReadUltra
          `mplus` parseINCommand openBot  OpenBotConnection
          `mplus` parseINCommand closeBot CloseBotConnection


-- Parser for multiple Statements
parseStatementExp :: Parser Statement
parseStatementExp = matchEnd $ ExpStatement <$> parseExp

-- Parser to easyfy structures like while loops and if expressions
parseStruct :: String -> (Exp -> Statement -> Statement) -> Parser Statement
parseStruct s c = do matchStr s
                     x <- parseExp
                     y <- parseBrackets $ parseStatement `mplus` return Empty
                     return $ c x y

-- Parser for an output command
parseOUTCommand :: String -> OUTCommand -> Parser Statement
parseOUTCommand s c = matchEnd $ fmap (Output c) (matchStr command >> matchStr s >> parseExp)

-- Parsre for an input command
parseINCommand :: String -> INCommand -> Parser Exp
parseINCommand s c = matchStr command >> matchStr s >> (return . Input) c

-- Parser for assignment of an expression
parseAssign :: Parser Statement
parseAssign = matchEnd $ assign' bool `mplus` assign' num
      where assign' t = do token t
                           s <- parseAlpha
                           matchStr assign
                           x <- parseExp
                           return (Assign s x)

-- Parser for a Statement
parseStatement :: Parser Statement
parseStatement = base `chainl1` return Statements
        where base = parseStatementExp
                    `mplus` parseAssign
                    `mplus` parseStruct      if'       If
                    `mplus` parseStruct      while     While
                    `mplus` parseOUTCommand  print'    Print
                    `mplus` parseOUTCommand  motorR    MotorRight
                    `mplus` parseOUTCommand  motorL    MotorLeft
