module Parser.BoolParser (parseBoolExp) where
import Control.Applicative
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base

--parse all boolean expressions--
parseBoolExp :: Parser BooleanExp
parseBoolExp =  parseLitBool
            `mplus` parseBoolVar
            `mplus` parseParens parseBoolExp
            `mplus` parseAltBinOPBool binaryAltBoolOp
            `chainl1` parseBinOPBool binaryBoolOp

--parse a literal boolean--
parseLitBool :: Parser BooleanExp
parseLitBool = createP1' true LitBool True `mplus` createP1' false LitBool False

parseBoolVar :: Parser BooleanExp
parseBoolVar = fmap BVar parseAlpha

--parse a --
parseUOPBool :: [(String,  UnaryBoolOp)] -> Parser BooleanExp
parseUOPBool     = parseFromTuple' parseU
parseU (s, cons) = do { matchStr s;
                        x <- parseParens parseBoolExp `mplus` parseLitBool;
                        (return . UnaryBoolOp cons) x;}


parseBinOPBool :: [(String, BinaryBoolOp)] -> Parser (BooleanExp -> BooleanExp -> BooleanExp)
parseBinOPBool     = parseFromTuple' parseBin
parseBin (s, cons) = createP1' s BinaryBoolOp cons

parseAltBinOPBool :: [(String, BinaryAltBoolOp)] -> Parser BooleanExp
parseAltBinOPBool     = parseFromTuple' parseAltBin
parseAltBin (s, cons) = do {x <- parseNumberExp;
                            matchStr s;
                            y <- parseNumberExp;
                            return (BinaryAltBoolOp cons x y) ;}
