module Parser.BoolParser (ParseBoolExp) where
import Parser.Base
import Parser.NumericParser

parseBoolExp :: Parser BooleanExpr
parseBoolExp = undefined


parseLitBool :: Parser BooleanExpr
parseLitBool = matchStr true `mplus` matchStr false
                where 
