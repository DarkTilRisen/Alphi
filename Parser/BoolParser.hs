module Parser.BoolParser (parseBoolExp) where
import Control.Applicative
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base


-- Parser for all boolean expressions
parseBoolExp :: Parser BooleanExp
parseBoolExp =  parseLitBool
            `mplus` parseBoolVar
            `mplus` parseParens parseBoolExp
            `mplus` parseAltBinOPBool binaryAltBoolOp
            `mplus` parseUOPBool uBoolOp
            `chainl1` parseBinOPBool binaryBoolOp

-- Parser for a literal boolean
parseLitBool :: Parser BooleanExp
parseLitBool =     createP1' true LitBool True
           `mplus` createP1' false LitBool False

-- Parser for a boolean variable
parseBoolVar :: Parser BooleanExp
parseBoolVar = token bool >> fmap BVar parseAlpha

--parser for an Unary operator
parseUOPBool :: [(String,  UnaryBoolOp)] -> Parser BooleanExp
parseUOPBool     = parseFromTuple' parseU
parseU (s, cons) = fmap (UnaryBoolOp cons)(matchStr s >>
                                          parseParens parseBoolExp
                                          `mplus` parseLitBool
                                          `mplus` parseBoolVar)


-- Parser for a binary boolean operator
parseBinOPBool :: [(String, BinaryBoolOp)] -> Parser (BooleanExp -> BooleanExp -> BooleanExp)
parseBinOPBool     = parseFromTuple' parseBin
parseBin (s, cons) = createP1' s BinaryBoolOp cons

-- Parser for a numeral expresion boolean operator
parseAltBinOPBool :: [(String, BinaryAltBoolOp)] -> Parser BooleanExp
parseAltBinOPBool     = parseFromTuple' parseAltBin
parseAltBin (s, cons) = do {x <- parseNumberExp;
                            matchStr s;
                            y <- parseNumberExp;
                            return (BinaryAltBoolOp cons x y) ;}
