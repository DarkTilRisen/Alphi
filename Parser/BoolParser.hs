module Parser.BoolParser where
import Control.Applicative (Alternative(..))
import Parser.NumericParser
import Control.Monad
import Parser.Base
import Parser.Util
import Data.Base



parseBoolExp :: Parser BooleanExpr
parseBoolExp = (parseLitBool
            `mplus` (parseUOPBool uBoolOp)) --chainl1 (parseBinOPBool binaryBoolOp)
          --  <|> (parseBinOPBool binaryBoolOp)
            `mplus` (parseAltBinOPBool binaryAltBoolOp)


parseLitBool :: Parser BooleanExpr
parseLitBool = createLit' true LitBool True `mplus` createLit' false LitBool False

parseUOPBool :: [(String,  UnaryBoolOp)] -> Parser BooleanExpr
parseUOPBool     = parseFromTuple parseU
parseU (s, cons) = do { matchStr s;
                              x <- parseBoolExp;
                              (return . UnaryBoolOp cons) x;}

parseBinOPBool :: [(String, BinaryBoolOp)] -> Parser (BooleanExpr -> BooleanExpr -> BooleanExpr)
parseBinOPBool     = parseFromTuple parseBin
parseBin (s, cons) = do { x <- parseBoolExp;
                          matchStr s;
                          y <- parseBoolExp;
                          return (BinaryBoolOp cons) ;}

parseAltBinOPBool :: [(String, BinaryAltBoolOp)] -> Parser BooleanExpr --(NumericExp -> NumericExp -> BooleanExpr)
parseAltBinOPBool     = parseFromTuple parseAltBin
parseAltBin (s, cons) = do {x <- parseNumberExp;
                            matchStr s;
                            y <- parseNumberExp;
                            return (BinaryAltBoolOp cons x y) ;}
