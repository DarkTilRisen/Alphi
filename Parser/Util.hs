module Parser.Util where
import Control.Applicative
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
parseString (x:xs)   = do y <- token x
                          parseString xs
                          return (y:xs)

parseAlpha :: Parser String
parseAlpha = parseTrailingSpace $ plus (spot isAlpha) >>= isKeyword
    where isKeyword x | x `elem` keywords = mzero
                      |  otherwise        = return x

-- Parser that ignores whiteSpace and newlines
parseWhiteSpace :: Parser String
parseWhiteSpace = plus $ spot isSpace <|> token '\n'

parseSpace :: Parser a -> Parser a
parseSpace = (=<<) $ \x ->  parseWhiteSpace >> return x

parseSpaceAndComments :: Parser String
parseSpaceAndComments = parseSpace parseComments

-- Creates a new parser that ignores newLines and whitespace
parseTrailingSpace :: Parser a -> Parser a
parseTrailingSpace p = parseSpace p `mplus` do {x <- parseSpace p;
                                                parseSpaceAndComments;
                                                return x }


--parseLeadingSpace :: Parser a -> Parser a
--parseLeadingSpace p = parseSpace p `mplus`


-- Match a certain keyword
matchStr :: String -> Parser String
matchStr = parseTrailingSpace  . parseString

-- Match parentheses
parseParens :: Parser a -> Parser a
parseParens p   = do matchStr parOpen
                     x <- p
                     matchStr parClosed
                     return x

-- Match
parseBrackets :: Parser a -> Parser a
parseBrackets p = do matchStr bracketsOpen
                     x <- p
                     matchStr bracketsClosed
                     return x

matchEnd :: Parser a -> Parser a
matchEnd p =  do x <- p
                 matchStr stop
                 return x

createP1 :: Parser a -> (b -> c) -> b -> Parser c
createP1 p c a1 = p >> (return . c) a1

createP1' :: String -> (a -> b) -> a -> Parser b
createP1' = createP1 . matchStr

parseComments :: Parser String
parseComments = do parseString commentOpen
                   findclose
                where findclose = parseString commentClose
                                  <|> (spot (const True) >> findclose)



parseFromTuple' :: (Functor t, Foldable t, MonadPlus m) => (a1 -> m a) -> t a1 -> m a
parseFromTuple' f xs  = foldl1 mplus $ fmap f xs

-- creates a new parser from a parser and a parser of an operator
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where rest a = do f <- op
                    b <- p
                    rest (f a b)
                    `mplus` return a
