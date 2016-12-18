module Data.Base where

data NumericExp       =   LitInteger      Int
                        | LitDouble       Double
                        | UnaryNumericOp  NumericBinaryOp NumericExp
                        | BinaryNumericOp NumericBinaryOp NumericExp NumericExp
                        deriving (Show, Eq)

data NumericBinaryOp = Add | Sub | Mul | Div | Mod deriving (Show, Eq)

data BooleanExpr     =   LitBool      Bool
                       | UnaryBoolOp  UnaryBoolOp BooleanExpr
                       | BinaryBoolOp BinaryBoolOp BooleanExpr BooleanExpr
                       | BinaryAltBoolOp BinaryAltBoolOp NumericExp NumericExp
                       deriving (Show, Eq)

data UnaryBoolOp     = Not deriving (Show, Eq)
data BinaryBoolOp    = And | Or deriving (Show, Eq)
data BinaryAltBoolOp = GreaterThan | SmallerThan | Equals deriving (Show, Eq)

data Statement       =  ExpB BooleanExpr
                      | ExpN NumericExp
                      | Statements Statement Statement
                      | If BooleanExpr Statement
                      | While BooleanExpr Statement deriving (Show, Eq)



-- Tokens --
parOpen        = "OPEN"  -- eq (
parClosed      = "CLOSE" -- eq )
bracketsOpen   = "START" -- eq {
bracketsClosed = "END" -- eq }
assign         = "IS"    -- eq =
floatSep       = "POINT" -- eq .
true           = "True"  -- eq true
false          = "False" -- eq false

--unary operators

--binary operators
add            = "ADD"
sub            = "SUB"
mult           = "MUL"
divide         = "DIV"
modulo         = "MOD"

--order of operations
--orderUNumOp = []
orderBNumOp = [[(mult, Mul), (divide, Div), (modulo, Mod)], [(add, Add), (sub, Sub)]]


--errors--
noParse        = "No parse was found!!!!!!"
ambiguousParse = "Parse is ambiguous!!!!!!"
