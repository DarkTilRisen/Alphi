module Data.Base where
import Data.Map
import System.HIDAPI hiding (error)
import Control.Monad.State

type Var              = String                                  -- typedef for variable
type Env a            = [(Var, a)]                              -- environment declaration
type MyState          = StateT (Env ReturnValue) IO ReturnValue -- rename this long type
emptyEnv              = []                                      -- create empty environment

-- variables that can be stored in the environment
data ReturnValue      = Num Double
                      | Boolean Bool
                      | Dev      Device
                      | Void

-- little numerical language
data NumericExp       = LitInteger      Int
                      | LitDouble       Double
                      | NVar            Var
                      | BinaryNumericOp NumericBinaryOp NumericExp NumericExp
                      deriving (Show, Eq)

-- operations used in the numerical language
data NumericBinaryOp  = Add
                      | Sub
                      | Mul
                      | Div
                      | Mod
                      deriving (Show, Eq)

-- little boolean language based upon the numerical language
data BooleanExp       = LitBool         Bool
                      | BVar            Var
                      | UnaryBoolOp     UnaryBoolOp     BooleanExp
                      | BinaryBoolOp    BinaryBoolOp    BooleanExp BooleanExp
                      | BinaryAltBoolOp BinaryAltBoolOp NumericExp NumericExp
                      deriving (Show, Eq)
-- unary operator for boolean expressions
data UnaryBoolOp     = Not deriving (Show, Eq)

-- binary operator for boolean expressions
data BinaryBoolOp    = And
                     | Or
                     deriving (Show, Eq)

data BinaryAltBoolOp = GreaterThan
                     | SmallerThan
                     | Equals
                     deriving (Show, Eq)

-- combination of multiple expressions
data Exp             = BExp   BooleanExp
                     | NExp   NumericExp
                     | Input  INCommand
                     deriving (Show, Eq)

-- statements
data Statement       = Empty
                     | Assign       Var       Exp
                     | Statements   Statement Statement
                     | If           Exp       Statement
                     | While        Exp       Statement
                     | ExpStatement Exp
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
                     | ReadUltra
                     | OpenBotConnection
                     | CloseBotConnection
                     deriving(Show, Eq)

-- keywords --
parOpen             = "Open"        -- eq (     --
parClosed           = "Close"       -- eq )     --
bracketsOpen        = "Begin"       -- eq {     --
bracketsClosed      = "End"         -- eq }     --
commentOpen         = "commentOpen" -- eq /*    --
commentClose        = "commentClose"-- eq */    --
assign              = "Is"          -- eq =     --
floatSep            = "Point"       -- eq .     --
true                = "True"        -- eq true  --
false               = "False"       -- eq false --
while               = "While"       -- eq while --
if'                 = "If"          -- eq  if   --
stop                = "Stop"        -- eq ;     --
command             = "Command"
print'              = "Print"
motorR              = "MotorR"
motorL              = "MotorL"
sensorL             = "SensorL"
sensorR             = "SensorR"
ultra               = "Ultra"
device              = "Device"
openBot            = "OpenMBot"
closeBot           = "CloseMBot"

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

orderBNumOp         =  [[(mul,  Mul),
                         (div', Div),
                         (mod', Mod)],
                        [(add, Add),
                         (sub, Sub)]]

boolLit             = [(true, True), (false, False)]
uBoolOp             = [(not', Not)]
binaryBoolOp        = [(and', And), (or', Or)]
binaryAltBoolOp     = [(gt, GreaterThan), (lt, SmallerThan), (eq, Equals)]

-- erros
noParse             = "No parse was found!!!!!!"
ambiguousParse      = "Parse is ambiguous!!!!!!"
evalerror           = "something went wrong when evaluating"
varNotFound         = "var was not found"
impossibleState     = "state not possible"
