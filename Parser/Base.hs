module Parser.Base where
import Control.Applicative (Alternative(..))
import Control.Monad
import Data.Base
import Data.List
--import Parser.NumericParsers (parseNumericExp)

newtype Parser a = Parser (String -> [(a, String)])

-- functor of a parser
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

-- monadPlus Defenition parser
instance MonadPlus Parser where
  mzero     = Parser (\s -> [])
  mplus m n = Parser (\s -> apply m s ++ apply n s)

-- Alternative of a parser
instance Alternative Parser where
   (<|>)     = option
   empty     = mzero

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> case apply p s of
                              []  -> apply q s
                              xs -> xs

failed :: Parser a
failed = Parser $ \s -> []

showParse :: (Show a) => Parser a -> String -> String
showParse m s = concat . intersperse ", " . map show $ apply m s

parse :: Parser a -> String -> [(a, String)]
parse m s = [ (x,t) | (x,t) <- apply m s, t == "" ]

-- Return parsed value, assuming at least one successful parse
parseResult :: Parser a -> String -> a
parseResult p s= one $ parse p s
  where
  one []                 = error noParse
  one [x]                = fst x
  one _                  = error ambiguousParse
