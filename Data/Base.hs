module Data.Base where
import Data.Map
import System.HIDAPI hiding (error)

type Var              = String
type Env a            = ([(Var, a)], Device)

data ReturnValue      = Num Double
                      | Boolean Bool
                      | IO      Device
                      | Void

data NumericExp       = LitInteger      Int
                      | LitDouble       Double
                      | NVar            Var
                      | BinaryNumericOp NumericBinaryOp NumericExp NumericExp
                      deriving (Show, Eq)

data NumericBinaryOp = Add
                     | Sub
                     | Mul
                     | Div
                     | Mod
                     deriving (Show, Eq)

data BooleanExp     = LitBool         Bool
                    | BVar            Var
                    | UnaryBoolOp     UnaryBoolOp     BooleanExp
                    | BinaryBoolOp    BinaryBoolOp    BooleanExp BooleanExp
                    | BinaryAltBoolOp BinaryAltBoolOp NumericExp NumericExp
                    deriving (Show, Eq)

data UnaryBoolOp     = Not deriving (Show, Eq)

data BinaryBoolOp    = And
                     | Or
                     deriving (Show, Eq)

data BinaryAltBoolOp = GreaterThan
                     | SmallerThan
                     | Equals
                     deriving (Show, Eq)

data Exp             = BExp   BooleanExp
                     | NExp   NumericExp
                     | Input  INCommand
                     deriving (Show, Eq)


data Statement       = ExpStatement Exp
                     | Assign       Var       Exp
                     | Statements   Statement Statement
                     | If           Exp       Statement
                     | While        Exp       Statement
                     | Output       OUTCommand   Exp
                      deriving (Show, Eq)

data OUTCommand      = Print
                     | MotorRight
                     | MotorLeft
                     | Led1
                     | Led2
                      deriving (Show, Eq)

data INCommand       = LineLeft
                     | LineRight
                     | ReadUltra deriving(Show, Eq)

-- keywords --
parOpen             = "OPEN"  -- eq (     --
parClosed           = "CLOSE" -- eq )     --
bracketsOpen        = "BEGIN" -- eq {     --
bracketsClosed      = "END"   -- eq }     --
assign              = "IS"    -- eq =     --
floatSep            = "POINT" -- eq .     --
true                = "TRUE"  -- eq true  --
false               = "FALSE" -- eq false --
while               = "WHILE"
if'                 = "IF"
stop                = "STOP"
command             = "COMMAND"
print'              = "PRINT"
motorR              = "MOTORR"
motorL              = "MOTORL"
sensorL             = "SENSORL"
sensorR             = "SENSORR"
ultra               = "ULTRA"

--types--
num                 = "NUM"
bool                = "BOOL"

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

keywords = [parOpen, parClosed, bracketsOpen, bracketsClosed, assign, floatSep
            , command, print', motorR, motorL, sensorL , sensorR, ultra
            , true, false, if', add, sub, mul, div', mod', not', and', or', gt, lt, eq]

--order of operations --
orderBNumOp         =  [[(mul,  Mul),
                         (div', Div),
                         (mod', Mod)],
                        [(add, Add),
                         (sub, Sub)]]

boolLit             = [(true, True), (false, False)]
uBoolOp             = [(not', Not)]
binaryBoolOp        = [(and', And), (or', Or)]
binaryAltBoolOp     = [(gt, GreaterThan), (lt, SmallerThan), (eq, Equals)]

--parsing errors --
noParse             = "No parse was found!!!!!!"
ambiguousParse      = "Parse is ambiguous!!!!!!"
evalerror           = "something went wrong when evaluating"
--eval errors --
varNotFound         = "var was not found"
impossibleState     = "state not possible"
