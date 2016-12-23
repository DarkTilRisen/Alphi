module Data.Base where
import Data.Map
import System.HIDAPI hiding (error)

type Var              = String
type Env a            = ([(Var, a)], Device)
type MyState          = StateT (Env ReturnValue) IO ReturnValue

data ReturnValue      = Num Double
                      | Boolean Bool
                      | IO      Device
                      | Void

data NumericExp       = LitInteger      Int
                      | LitDouble       Double
                      | NVar            Var
                      | BinaryNumericOp NumericBinaryOp NumericExp NumericExp
                      deriving (Show, Eq)

data NumericBinaryOp  = Add
                      | Sub
                      | Mul
                      | Div
                      | Mod
                      deriving (Show, Eq)

data BooleanExp       = LitBool         Bool
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
parOpen             = "Open"  -- eq (     --
parClosed           = "Close" -- eq )     --
bracketsOpen        = "Begin" -- eq {     --
bracketsClosed      = "End"   -- eq }     --
assign              = "Is"    -- eq =     --
floatSep            = "Point" -- eq .     --
true                = "True"  -- eq true  --
false               = "False" -- eq false --
while               = "While"
if'                 = "If"
stop                = "Stop"
command             = "Command"
print'              = "Print"
motorR              = "MotorR"
motorL              = "MotorL"
sensorL             = "SensorL"
sensorR             = "SensorR"
ultra               = "Ultra"

--types--
num                 = 'N'
bool                = 'B'

-- binary numeric operators --
add                 = "Add"
sub                 = "Sub"
mul                 = "Mul"
div'                = "Div"
mod'                = "Mod"

-- unary boolean operators --
not'                = "Not"

-- binary boolean operators --
and'                = "And"
or'                 = "Or"
gt                  = "Gt"
lt                  = "Lt"
eq                  = "Eq"

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
