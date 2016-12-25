module Parser.Base where
import Control.Applicative
import Control.Monad
import Data.Base
import Data.List

-- Define new parser type
newtype Parser a = Parser (String -> [(a, String)])

-- Functor of a parser
instance Functor Parser where
  fmap = liftM

-- Applicative of a parser
instance Applicative Parser where
  pure   = return
  (<*>)  = ap

-- monad Defenition parser
instance Monad Parser where
  return x = Parser (\s -> [(x,s)])
  m >>= k  = Parser (\s -> [ (y, u) | (x, t) <- apply m s, (y, u) <- apply (k x) t ])

-- MonadPlus Defenition parser
instance MonadPlus Parser where
  mzero     = Parser $ const []
  mplus m n = Parser (\s -> apply m s ++ apply n s)

-- Alternative of a parser
instance Alternative Parser where
   empty     = mzero
   (<|>)     = option


-- Apply a parser.
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

-- Implement option for an alternative
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> case apply p s of
                              []  -> apply q s
                              xs -> xs


-- Return the parse from a parser
parse :: Parser a -> String -> [(a, String)]
parse m s = [ (x,t) | (x,t) <- apply m s, t == "" ]

-- Return parsed value, assuming at least one successful parse
parseResult :: Parser a -> String -> a
parseResult p s          = one $ parse p s
  where
  one []                 = error noParse
  one [x]                = fst x
  one _                  = error ambiguousParse
