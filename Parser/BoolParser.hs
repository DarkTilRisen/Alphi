module Parser.BoolParser where
import Control.Applicative (Alternative(..))
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base


--parse all boolean expressions--
parseBoolExp :: Parser BooleanExpr
parseBoolExp =  parseBoolAssign
            <|> parseLitBool
            <|> parseBoolVar
            <|> (parseUOPBool uBoolOp)
            <|> (parseAltBinOPBool binaryAltBoolOp)
            `chainl1` (parseBinOPBool binaryBoolOp)

--parse a literal boolean--
parseLitBool :: Parser BooleanExpr
parseLitBool = createP1' true LitBool True `mplus` createP1' false LitBool False

--parse a boolean assignment--
parseBoolAssign :: Parser BooleanExpr
parseBoolAssign = matchStr bool >> parseAssign parseBoolExp BAssign

parseBoolVar :: Parser BooleanExpr
parseBoolVar = fmap BVar parseAlpha

--parse a --
parseUOPBool :: [(String,  UnaryBoolOp)] -> Parser BooleanExpr
parseUOPBool     = parseFromTuple' parseU
parseU (s, cons) = do { matchStr s;
                        x <- (parseParens parseBoolExp) `mplus` parseLitBool;
                        (return . UnaryBoolOp cons) x;}


parseBinOPBool :: [(String, BinaryBoolOp)] -> Parser (BooleanExpr -> BooleanExpr -> BooleanExpr)
parseBinOPBool     = parseFromTuple' parseBin
parseBin (s, cons) = createP1' s BinaryBoolOp cons

parseAltBinOPBool :: [(String, BinaryAltBoolOp)] -> Parser BooleanExpr
parseAltBinOPBool     = parseFromTuple' parseAltBin
parseAltBin (s, cons) = do {x <- parseNumberExp;
                            matchStr s;
                            y <- parseNumberExp;
                            return (BinaryAltBoolOp cons x y) ;}
