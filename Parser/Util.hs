module Parser.Util where
import Control.Applicative (Alternative(..))
import Control.Monad
import Parser.Base
import Data.Base
import Data.Char



-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = do x  <- p
            xs <- star p
            return (x:xs)

-- parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do c <- char
            guard (p c)
            return c

-- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

-- parse exactly one character
char :: Parser Char
char = Parser f
  where
  f ""      = []
  f (c:s)  = [(c,s)]


parseString :: String -> Parser String
parseString ""       = return ""
parseString (x:xs)   = do { y <- token x;
                            parseString xs;
                            return (y:xs)}

  -- parses whiteSpace and newlines
parseWhiteSpace :: Parser String
parseWhiteSpace = star $ spot isSpace <|> token '\n'

parseTrailingSpace :: Parser a -> Parser a
parseTrailingSpace =  flip (>>=) f
                        where f x = parseWhiteSpace >> return x

matchStr :: String -> Parser String
matchStr = parseTrailingSpace . parseString

parseParens :: Parser a -> Parser a
parseParens p = do {matchStr parOpen ; x <- p ;matchStr parClosed; return x }
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
  where rest a = do {f <- op;b <- p;rest (f a b)} `mplus` return a
