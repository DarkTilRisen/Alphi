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



-- keywords --
parOpen             = "OPEN"  -- eq (     --
parClosed           = "CLOSE" -- eq )     --
bracketsOpen        = "START" -- eq {     --
bracketsClosed      = "END"   -- eq }     --
assign              = "IS"    -- eq =     --
floatSep            = "POINT" -- eq .     --
true                = "TRUE"  -- eq true  --
false               = "FALSE" -- eq false --
while               = "WHILE"
if'                 = "IF"

-- unary numeric operators --

-- binary numeric operators --
add                 = "ADD"
sub                 = "SUB"
mul                 = "MUL"
div'                = "DIV"
mod'                = "MOD"

-- unary boolean operators --
not'                = "NOT"

-- binary boolean operators --
and'                = "AND"
or'                 = "OR"
gt                  = "GT"
lt                  = "LT"
eq                  = "EQ"

--data Hierarchy a = Const Hierarchy 
--order of operations --
orderBNumOp         =  [[(mul,  Mul),
                         (div', Div),
                         (mod', Mod)],
                        [(add, Add),
                         (sub, Sub)]]

uBoolOp             = [(not', Not)]
binaryBoolOp        = [(and', And), (or', Or)]
binaryAltBoolOp     = [(gt, GreaterThan), (lt, SmallerThan), (eq, Equals)]


--errors--
noParse        = "No parse was found!!!!!!"
ambiguousParse = "Parse is ambiguous!!!!!!"
