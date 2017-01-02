Main.hs

0 module Main where
1
2 import Data.Char
3 import Data.Base
4 import Parser.Base
5 import Control.Monad.State
6 import Parser.StatementParser
7 import Evaluator.StatementEval
8 import Parser.Util
9 import Control.Applicative
10
11
12 -- parses a string to a Statement
13 parseAll :: String -> Statement
14 parseAll = parseResult finalParse
15         where finalParse = parseLeadingSpace >> parseStatement
16
17 -- parses and evaluates a given string
18 eval :: String -> IO (ReturnValue, Env ReturnValue)
19 eval = flip (runStateT . evalStatement . parseAll) emptyEnv
20
21 --run the parser and evaluator from a given file.
22 main :: IO ()
23 main = readFile "AlphiExamples/demo_exp.alp" >>= eval >> print "done"


Base.hs

0 module Parser.Base where
1 import Control.Applicative
2 import Control.Monad
3 import Data.Base
4 import Data.List
5
6 -- Define new parser type
7 newtype Parser a = Parser (String -> [(a, String)])
8
9 -- Functor of a parser
10 instance Functor Parser where
11   fmap = liftM
12
13 -- Applicative of a parser
14 instance Applicative Parser where
15   pure   = return
16   (<*>)  = ap
17
18 -- monad Defenition parser
19 instance Monad Parser where
20   return x = Parser (\s -> [(x,s)])
21   m >>= k  = Parser (\s -> [ (y, u) | (x, t) <- apply m s, (y, u) <- apply (k x) t ])
22
23 -- MonadPlus Defenition parser
24 instance MonadPlus Parser where
25   mzero     = Parser $ const []
26   mplus m n = Parser (\s -> apply m s ++ apply n s)
27
28 -- Alternative of a parser
29 instance Alternative Parser where
30    empty     = mzero
31    (<|>)     = option
32
33
34 -- Apply a parser.
35 apply :: Parser a -> String -> [(a, String)]
36 apply (Parser f) = f
37
38 -- Implement option for an alternative
39 option :: Parser a -> Parser a -> Parser a
40 option p q = Parser $ \s -> case apply p s of
41                               []  -> apply q s
42                               xs -> xs
43
44
45 -- Return the parse from a parser
46 parse :: Parser a -> String -> [(a, String)]
47 parse m s = [ (x,t) | (x,t) <- apply m s, t == "" ]
48
49 -- Return parsed value, assuming at least one successful parse
50 parseResult :: Parser a -> String -> a
51 parseResult p s          = one $ parse p s
52   where
53   one []                 = error noParse
54   one [x]                = fst x
55   one _                  = error ambiguousParse


BoolParser.hs

0 module Parser.BoolParser (parseBoolExp) where
1 import Control.Applicative
2 import Parser.NumericParser
3 import Control.Monad
4 import Parser.Base
5 import Parser.Util
6 import Data.Base
7
8
9 -- Parser for all boolean expressions
10 parseBoolExp :: Parser BooleanExp
11 parseBoolExp =  base `chainl1` parseBinOPBool binaryBoolOp
12             where base =      parseLitBool
13                       `mplus` parseBoolVar
14                       `mplus` parseParens parseBoolExp
15                       `mplus` parseAltBinOPBool binaryAltBoolOp
16                       `mplus` parseUOPBool uBoolOp
17
18 -- Parser for a literal boolean
19 parseLitBool :: Parser BooleanExp
20 parseLitBool =     createP1' true LitBool True
21            `mplus` createP1' false LitBool False
22
23 -- Parser for a boolean variable
24 parseBoolVar :: Parser BooleanExp
25 parseBoolVar = token bool >> fmap BVar parseAlpha
26
27 --parser for an Unary operator
28 parseUOPBool :: [(String,  UnaryBoolOp)] -> Parser BooleanExp
29 parseUOPBool     = parseFromTuple' parseU
30 parseU (s, cons) = fmap (UnaryBoolOp cons)(matchStr s >>
31                                           parseParens parseBoolExp
32                                           `mplus` parseLitBool
33                                           `mplus` parseBoolVar)
34
35
36 -- Parser for a binary boolean operator
37 parseBinOPBool :: [(String, BinaryBoolOp)] -> Parser (BooleanExp -> BooleanExp -> BooleanExp)
38 parseBinOPBool     = parseFromTuple' parseBin
39 parseBin (s, cons) = createP1' s BinaryBoolOp cons
40
41 -- Parser for a numeral expresion boolean operator
42 parseAltBinOPBool :: [(String, BinaryAltBoolOp)] -> Parser BooleanExp
43 parseAltBinOPBool     = parseFromTuple' parseAltBin
44 parseAltBin (s, cons) = do  x <- parseNumberExp
45                             matchStr s
46                             y <- parseNumberExp
47                             return (BinaryAltBoolOp cons x y)


NumericParser.hs

0 module Parser.NumericParser (parseNumberExp) where
1 import Control.Applicative
2 import Control.Monad
3 import Parser.Base
4 import Data.Char
5 import Parser.Util
6 import Data.Base
7
8 -- Parser for a series of digits
9 parseDigits :: Parser String
10 parseDigits = plus $ spot isDigit
11
12 -- Parser for an integer
13 parseInt :: Parser Int
14 parseInt = fmap read parseDigits
15
16 -- Parser for doubles
17 parseDouble :: Parser Double
18 parseDouble = do  x <- parseDigits;
19                   parseString floatSep;
20                   y <- parseDigits;
21                   return (read (x ++ "." ++ y) :: Double)
22
23 -- Parser for an numerical variable
24 parseNumVar :: Parser NumericExp
25 parseNumVar = token num >> fmap NVar parseAlpha
26
27 -- Parser for an numeral
28 parseNumLiteral :: Parser NumericExp
29 parseNumLiteral = parseTrailingSpace $ fmap LitInteger parseInt
30                                `mplus` fmap LitDouble parseDouble
31
32
33 -- Function to make order of expressions easier
34 chainExp:: Parser NumericExp -> [(String, NumericBinaryOp)] -> Parser NumericExp
35 chainExp acc xs = chainl1 acc $ parseFromTuple' f xs
36                     where f (s, cons) = createP1' s BinaryNumericOp cons
37
38 -- Parser for all numerical expressions
39 parseNumberExp :: Parser NumericExp
40 parseNumberExp = foldl chainExp base orderBNumOp
41                     where base =        parseNumVar
42                                 `mplus` parseNumLiteral
43                                 `mplus` parseParens parseNumberExp


StatementParser.hs

0 module Parser.StatementParser (parseStatement) where
1 import Control.Applicative
2 import Parser.NumericParser
3 import Control.Monad
4 import Parser.Base
5 import Parser.Util
6 import Data.Base
7 import Parser.BoolParser
8
9 -- Parser for an Expression
10 parseExp :: Parser Exp
11 parseExp =        fmap BExp parseBoolExp
12           `mplus` fmap NExp parseNumberExp
13           `mplus` parseINCommand sensorL  LineLeft
14           `mplus` parseINCommand sensorR  LineRight
15           `mplus` parseINCommand ultra    ReadUltra
16           `mplus` parseINCommand openBot  OpenBotConnection
17           `mplus` parseINCommand closeBot CloseBotConnection
18
19
20 -- Parser for multiple Statements
21 parseStatementExp :: Parser Statement
22 parseStatementExp = matchEnd $ ExpStatement <$> parseExp
23
24 -- Parser to easyfy structures like while loops and if expressions
25 parseStruct :: String -> (Exp -> Statement -> Statement) -> Parser Statement
26 parseStruct s c = do matchStr s
27                      x <- parseExp
28                      y <- parseBrackets $ parseStatement `mplus` return Empty
29                      return $ c x y
30
31 -- Parser for an output command
32 parseOUTCommand :: String -> OUTCommand -> Parser Statement
33 parseOUTCommand s c = matchEnd $ fmap (Output c) (matchStr command >> matchStr s >> parseExp)
34
35 -- Parsre for an input command
36 parseINCommand :: String -> INCommand -> Parser Exp
37 parseINCommand s c = matchStr command >> matchStr s >> (return . Input) c
38
39 -- Parser for assignment of an expression
40 parseAssign :: Parser Statement
41 parseAssign = matchEnd $ assign' bool `mplus` assign' num
42       where assign' t = do token t
43                            s <- parseAlpha
44                            matchStr assign
45                            x <- parseExp
46                            return (Assign s x)
47
48 -- Parser for a Statement
49 parseStatement :: Parser Statement
50 parseStatement = base `chainl1` return Statements
51         where base = parseStatementExp
52                     `mplus` parseAssign
53                     `mplus` parseStruct      if'       If
54                     `mplus` parseStruct      while     While
55                     `mplus` parseOUTCommand  print'    Print
56                     `mplus` parseOUTCommand  motorR    MotorRight
57                     `mplus` parseOUTCommand  motorL    MotorLeft
58                     `mplus` parseOUTCommand  led1      Led1
59                     `mplus` parseOUTCommand  led2      Led2


Util.hs

0 module Parser.Util where
1 import Control.Applicative
2 import Control.Monad
3 import Parser.Base
4 import Data.Base
5 import Data.Char
6
7 -- match zero or more occurrences
8 star :: Parser a -> Parser [a]
9 star p = plus p `mplus` return []
10
11 -- match one or more occurrences
12 plus :: Parser a -> Parser [a]
13 plus p = do x  <- p
14             xs <- star p
15             return (x:xs)
16
17 -- parse a character satisfying a predicate (e.g., isDigit)
18 spot :: (Char -> Bool) -> Parser Char
19 spot p = do c <- char
20             guard (p c)
21             return c
22
23 -- Match a given character
24 token :: Char -> Parser Char
25 token c = spot (== c)
26
27 -- parse exactly one character
28 char :: Parser Char
29 char = Parser f
30   where
31   f ""      = []
32   f (c:s)  = [(c,s)]
33
34
35 parseString :: String -> Parser String
36 parseString ""       = return ""
37 parseString (x:xs)   = do y <- token x
38                           parseString xs
39                           return (y:xs)
40
41 parseAlpha :: Parser String
42 parseAlpha = parseTrailingSpace $ plus (spot isAlpha) >>= isKeyword
43     where isKeyword x | x `elem` keywords = mzero
44                       |  otherwise        = return x
45
46 -- Parser that ignores whiteSpace and newlines
47 parseWhiteSpace :: Parser Char
48 parseWhiteSpace = spot isSpace <|> token '\n'
49
50 parseSpace :: Parser a -> Parser a
51 parseSpace = (=<<) $ \x ->  plus parseWhiteSpace >> return x
52
53 parseSpaceAndComments :: Parser String
54 parseSpaceAndComments = parseSpace parseComments
55
56 -- Creates a new parser that ignores newLines and whitespace
57 parseTrailingSpace :: Parser a -> Parser a
58 parseTrailingSpace p = parseSpace p `mplus` do  x <- parseSpace p
59                                                 parseSpaceAndComments
60                                                 return x
61
62
63 parseLeadingSpace ::  Parser String
64 parseLeadingSpace = star parseWhiteSpace `mplus` (star parseWhiteSpace >> parseSpaceAndComments)
65
66
67 -- Match a certain keyword
68 matchStr :: String -> Parser String
69 matchStr = parseTrailingSpace  . parseString
70
71 -- Match parentheses
72 parseParens :: Parser a -> Parser a
73 parseParens p   = do matchStr parOpen
74                      x <- p
75                      matchStr parClosed
76                      return x
77
78 -- Match
79 parseBrackets :: Parser a -> Parser a
80 parseBrackets p = do matchStr bracketsOpen
81                      x <- p
82                      matchStr bracketsClosed
83                      return x
84
85 matchEnd :: Parser a -> Parser a
86 matchEnd p =  do x <- p
87                  matchStr stop
88                  return x
89
90 createP1 :: Parser a -> (b -> c) -> b -> Parser c
91 createP1 p c a1 = p >> (return . c) a1
92
93 createP1' :: String -> (a -> b) -> a -> Parser b
94 createP1' = createP1 . matchStr
95
96 parseComments :: Parser String
97 parseComments = do parseString commentOpen
98                    findclose
99                 where findclose = parseString commentClose
100                                   <|> (spot (const True) >> findclose)
101
102
103
104 parseFromTuple' :: (Functor t, Foldable t, MonadPlus m) => (a1 -> m a) -> t a1 -> m a
105 parseFromTuple' f xs  = foldl1 mplus $ fmap f xs
106
107 -- creates a new parser from a parser and a parser of an operator
108 chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
109 chainl1 p op = p >>= rest
110   where rest a = do f <- op
111                     b <- p
112                     rest (f a b)
113                     `mplus` return a


NumericEval.hs

0 module Evaluator.NumericEval (evalNumExp) where
1 import Control.Monad.State
2 import Control.Monad
3 import Data.Maybe
4 import Parser.Base
5 import Control.Monad.Trans.Maybe
6 import Parser.NumericParser
7 import Parser.BoolParser
8 import Data.Base
9 import Parser.Util
10 import Parser.StatementParser
11 import Evaluator.Util
12
13 -- evaluate boolean expressions
14 evalNumExp :: NumericExp -> MyState
15 evalNumExp (LitDouble x)             = (return . Num ) x
16 evalNumExp (LitInteger x)            = (return . Num . fromIntegral) x
17 evalNumExp (BinaryNumericOp Add x y) = evalBOp (+)  x y evalNumExp getNum Num
18 evalNumExp (BinaryNumericOp Sub x y) = evalBOp (-)  x y evalNumExp getNum Num
19 evalNumExp (BinaryNumericOp Mul x y) = evalBOp (*)  x y evalNumExp getNum Num
20 evalNumExp (BinaryNumericOp Div x y) = evalBOp (/)  x y evalNumExp getNum Num
21 evalNumExp (BinaryNumericOp Mod x y) = evalBOp mod'' x y evalNumExp getNum Num
22 evalNumExp (NVar x)                  = state $ \s -> (find x s,s)
23
24
25 -- what a stupid hack
26 mod'' :: Double -> Double -> Double
27 mod'' x y  = fromIntegral $ mod (round x ) (round y)


StatementEval.hs

0 module Evaluator.StatementEval (evalStatement) where
1 import Data.Base
2 import Control.Monad.State
3 import Evaluator.Util
4 import Evaluator.BoolEval
5 import Evaluator.NumericEval
6 import Robot.Base
7 import System.HIDAPI (Device)
8 import MBot hiding (Command)
9
10
11 --Evaluate expressions
12 evalExp :: Exp -> MyState
13 evalExp (BExp  e) = evalBoolExp e
14 evalExp (NExp  e) = evalNumExp  e
15 evalExp (Input c) = evalInput   c
16
17 -- Evaluate input commands
18 evalInput :: INCommand -> MyState
19 evalInput ReadUltra          = evalInput' readUltra Num
20 evalInput LineLeft           = evalInput' (readLine SensorL) Boolean
21 evalInput LineRight          = evalInput' (readLine SensorR) Boolean
22 evalInput OpenBotConnection  = fmap Dev (liftIO openMBot) >>= insert device
23 evalInput CloseBotConnection = returnDevice >>= liftIO . closeMBot . getDevice >> return Void
24
25 -- Evaluate statements
26 evalStatement :: Statement -> MyState
27 evalStatement (ExpStatement e)   = evalExp e
28 evalStatement (Statements s1 s2) = evalStatement s1 >> evalStatement s2
29 evalStatement (If b s)           = evalStruct b s $ evalStatement s
30 evalStatement (While b s)        = evalStruct b s $ evalStatement s >> evalStatement (While b s)
31 evalStatement (Assign s e)       = evalAssign s e
32 evalStatement (Output t e)       = evalCommand t e
33 evalStatement Empty              = return Void
34
35 -- Evaluate output commands
36 evalCommand :: OUTCommand -> Exp -> MyState
37 evalCommand Print (BExp e)       = evalPrint (evalBoolExp e) getBool
38 evalCommand Print (NExp e)       = evalPrint (evalNumExp e)  getNum
39 evalCommand MotorRight e         = evalRobotFunction e MotorR move
40 evalCommand MotorLeft  e         = evalRobotFunction e MotorL move
41 evalCommand Data.Base.Led1 e     = evalRobotFunction e Robot.Base.Led1 led
42 evalCommand Data.Base.Led2 e     = evalRobotFunction e Robot.Base.Led2 led
43
44
45 -- Evaluate input
46 evalInput' :: (Device -> IO a) -> (a -> ReturnValue) -> MyState
47 evalInput' f c = fmap c (returnDevice >>= liftIO . f . getDevice)
48 -- Evaluate print commands
49 evalPrint :: (Show a) => MyState -> (ReturnValue -> a) -> MyState
50 evalPrint e f = e >>= (liftIO . print . f) >> return Void
51
52 evalRobotFunction :: Integral c => Exp -> t -> (c -> t -> Device -> IO a) -> StateT (Env ReturnValue) IO ReturnValue
53 evalRobotFunction e l f = do x <- evalExp e
54                              d <- returnDevice;
55                              liftIO (f ((floor . getNum) x) l (getDevice d))
56                              return Void
57
58 -- Evaluate structures like while and if
59 evalStruct :: Exp -> Statement -> MyState -> MyState
60 evalStruct b s f = evalExp b >>= check
61   where check (Boolean True)  = f
62         check (Boolean False) = return Void
63         check x               = error impossibleState
64
65 -- Evaluate assignment
66 evalAssign :: String -> Exp -> MyState
67 evalAssign st e = evalExp e >>= insert st


BoolEval.hs

0 module Evaluator.BoolEval (evalBoolExp) where
1
2 import Control.Monad.State
3 import Data.Base
4 import Evaluator.Util
5 import Evaluator.NumericEval
6
7
8 -- evaluate boolean expressions
9 evalBoolExp :: BooleanExp -> MyState
10 evalBoolExp (LitBool x)                          = return (Boolean x)
11 evalBoolExp (UnaryBoolOp Not x)                  = fmap (Boolean . not . getBool) (evalBoolExp x)
12 evalBoolExp (BinaryBoolOp And x y)               = evalBOp (&&) x y evalBoolExp getBool Boolean
13 evalBoolExp (BinaryBoolOp Or  x y)               = evalBOp (||) x y evalBoolExp getBool Boolean
14 evalBoolExp (BinaryAltBoolOp GreaterThan x y)    = evalBOp (>)  x y evalNumExp getNum Boolean
15 evalBoolExp (BinaryAltBoolOp SmallerThan x y)    = evalBOp (<)  x y evalNumExp getNum Boolean
16 evalBoolExp (BinaryAltBoolOp Equals x y )        = evalBOp (==) x y evalNumExp getNum Boolean
17 evalBoolExp (BVar x)                             = state $ \s -> (find x s, s)


Util.hs

0 module Evaluator.Util where
1 import Control.Monad.State
2 import Data.Base
3 import Data.Maybe
4 import System.HIDAPI hiding (error)
5
6 evalBOp :: Monad m => (t3 -> t3 -> t2) -> t -> t -> (t -> m t1) -> (t1 -> t3) -> (t2 -> b) -> m b
7 evalBOp f x y e g c = do x' <- e x
8                          y' <- e y
9                          return (c (f (g x') (g y')))
10
11 find ::(Eq a) => a -> [(a,b)] -> b
12 find x env = fromMaybe (error varNotFound) (lookup x env)
13
14
15 insertVar :: String -> a -> Env a -> Env a
16 insertVar s a env =  (s,a):remove s env
17   where remove s  = filter (\(s1,a) -> (s1 /= s))
18
19
20 -- Insert a return value
21 insert :: String -> ReturnValue ->  MyState
22 insert st x = state $ \s -> (x, insertVar st x s)
23
24 -- Get a variable
25 getVar :: String -> Env a -> (a -> b) -> b
26 getVar s env f = f $ find s env
27
28 -- Return the device from the state
29 returnDevice :: MyState
30 returnDevice =  state $ \s -> (find device s, s)
31
32
33 getDevice :: ReturnValue -> Device
34 getDevice (Dev d) = d
35 getDevice x       = error impossibleState
36
37 -- Extract a double from a return value
38 getNum :: ReturnValue -> Double
39 getNum (Num x)    = x
40 getNum x          = error impossibleState
41
42 -- Extract a boolean from a return value
43 getBool :: ReturnValue -> Bool
44 getBool (Boolean x) = x
45 getBool x           = error impossibleState


robot.hs

0 import MBot
1
2 -- short linefollowing program
3 a d x=maybe(stop d)($d)$lookup x[(LEFTB,goLeft),(RIGHTB,goRight),(BOTHB,goAhead),(BOTHW,goBackwards)]
4 r d i =readLineFollower d>>=a d>> print i >>r d (i + 1)
5 main=openMBot>>= \x -> r x 0


robotstop.hs

0 import MBot
1
2 --program to stop the motors of the Mbot
3 main :: IO ()
4 main = do {
5   d <- openMBot;
6   stop d;
7   closeMBot d
8 }


Base.hs

0 -- A more intuitive library
1 module Robot.Base where
2 import System.HIDAPI hiding (error)
3 import MBot
4 import qualified Data.Base as Data
5 import Data.Bits
6 import Evaluator.Util
7 import GHC.Float
8
9 motors     = [(MotorR, 0xa), (MotorL, 0x9)]
10 linesensor = [(SensorL, LEFTB), (SensorR, RIGHTB )]
11 leds       = [(Led1, 1), (Led2, 2)]
12 stops      = 0
13
14 data Motor       = MotorL  | MotorR  deriving (Eq)
15 data LineSensor  = SensorL | SensorR deriving (Eq)
16 data Led         = Led1    | Led2    deriving (Eq)
17
18
19 -- Note I have no clue how this works !!!!!!
20 -- A more generic and intuitive implementation to control the motors --
21 -- Speed range [-255,255]
22 move :: Int -> Motor -> Device -> IO ()
23 move s m d | m == MotorR && s > 0 = move' s stops
24            | m == MotorR          = move' (complement (-s)) (complement stops)
25            | m == MotorL && s > 0 = move' (complement s) (complement stops)
26            | m == MotorL          = move' (-s) stops
27            | otherwise            = error Data.impossibleState
28           where move' s x = sendCommand d $ setMotor (find m motors) s x
29
30 {-
31 1 -> Red
32 2 -> Green
33 3 -> Blue
34 -}
35
36 led :: Int -> Led -> Device -> IO()
37 led x l d | x == 0    = f 0   0   0
38           | x == 1    = f 100 0   0
39           | x == 2    = f 0   100 0
40           | x == 3    = f 0   0   100
41           | otherwise = error Data.impossibleState
42           where f r g b =  sendCommand d $ setRGB (find l leds) b g r
43
44 --Read out the ultrasonic sensor and convert it to a IO(Double) --
45 readUltra :: Device -> IO Double
46 readUltra d = fmap float2Double (readUltraSonic d)
47
48 -- check if the sensor sees white or black --
49 readLine :: LineSensor -> Device -> IO Bool
50 readLine s d = readLineFollower d >>= match
51             where match LEFTB  = return $ find s linesensor == LEFTB
52                   match RIGHTB = return $ find s linesensor == RIGHTB
53                   match BOTHB  = return True
54                   match BOTHW  = return False


Base.hs

0 module Data.Base where
1 import Data.Map
2 import System.HIDAPI hiding (error)
3 import Control.Monad.State
4
5 type Var              = String                                  -- typedef for variable
6 type Env a            = [(Var, a)]                              -- environment declaration
7 type MyState          = StateT (Env ReturnValue) IO ReturnValue -- rename this long type
8 emptyEnv              = []                                      -- create empty environment
9
10 -- variables that can be stored in the environment
11 data ReturnValue      = Num Double
12                       | Boolean Bool
13                       | Dev      Device
14                       | Void
15
16 -- little numerical language
17 data NumericExp       = LitInteger      Int
18                       | LitDouble       Double
19                       | NVar            Var
20                       | BinaryNumericOp NumericBinaryOp NumericExp NumericExp
21                       deriving (Show, Eq)
22
23 -- operations used in the numerical language
24 data NumericBinaryOp  = Add
25                       | Sub
26                       | Mul
27                       | Div
28                       | Mod
29                       deriving (Show, Eq)
30
31 -- little boolean language based upon the numerical language
32 data BooleanExp       = LitBool         Bool
33                       | BVar            Var
34                       | UnaryBoolOp     UnaryBoolOp     BooleanExp
35                       | BinaryBoolOp    BinaryBoolOp    BooleanExp BooleanExp
36                       | BinaryAltBoolOp BinaryAltBoolOp NumericExp NumericExp
37                       deriving (Show, Eq)
38 -- unary operator for boolean expressions
39 data UnaryBoolOp     = Not deriving (Show, Eq)
40
41 -- binary operator for boolean expressions
42 data BinaryBoolOp    = And
43                      | Or
44                      deriving (Show, Eq)
45
46 data BinaryAltBoolOp = GreaterThan
47                      | SmallerThan
48                      | Equals
49                      deriving (Show, Eq)
50
51 -- combination of multiple expressions
52 data Exp             = BExp   BooleanExp
53                      | NExp   NumericExp
54                      | Input  INCommand
55                      deriving (Show, Eq)
56
57 -- statements
58 data Statement       = Empty
59                      | Assign       Var       Exp
60                      | Statements   Statement Statement
61                      | If           Exp       Statement
62                      | While        Exp       Statement
63                      | ExpStatement Exp
64                      | Output       OUTCommand   Exp
65                       deriving (Show, Eq)
66
67 data OUTCommand      = Print
68                      | MotorRight
69                      | MotorLeft
70                      | Led1
71                      | Led2
72
73                       deriving (Show, Eq)
74
75 data INCommand       = LineLeft
76                      | LineRight
77                      | ReadUltra
78                      | OpenBotConnection
79                      | CloseBotConnection
80                      deriving(Show, Eq)
81
82 -- keywords --
83 parOpen             = "Open"        -- eq (     --
84 parClosed           = "Close"       -- eq )     --
85 bracketsOpen        = "Begin"       -- eq {     --
86 bracketsClosed      = "End"         -- eq }     --
87 commentOpen         = "commentOpen" -- eq /*    --
88 commentClose        = "commentClose"-- eq */    --
89 assign              = "Is"          -- eq =     --
90 floatSep            = "Point"       -- eq .     --
91 true                = "True"        -- eq true  --
92 false               = "False"       -- eq false --
93 while               = "While"       -- eq while --
94 if'                 = "If"          -- eq  if   --
95 stop                = "Stop"        -- eq ;     --
96 command             = "Command"
97 print'              = "Print"
98 motorR              = "MotorR"
99 motorL              = "MotorL"
100 sensorL             = "SensorL"
101 sensorR             = "SensorR"
102 led1                = "Led1"
103 led2                = "Led2"
104 ultra               = "Ultra"
105 device              = "Device"
106 openBot             = "OpenMBot"
107 closeBot            = "CloseMBot"
108
109 --types--
110 num                 = 'N'
111 bool                = 'B'
112
113 -- binary numeric operators --
114 add                 = "Add"
115 sub                 = "Sub"
116 mul                 = "Mul"
117 div'                = "Div"
118 mod'                = "Mod"
119
120 -- unary boolean operators --
121 not'                = "Not"
122
123 -- binary boolean operators --
124 and'                = "And"
125 or'                 = "Or"
126 gt                  = "Gt"
127 lt                  = "Lt"
128 eq                  = "Eq"
129
130 keywords = [parOpen, parClosed, bracketsOpen, bracketsClosed, assign, floatSep
131             , command, print', motorR, motorL, sensorL , sensorR, ultra, true
132             , false, if', add, sub, mul, div', mod', not', and', or', gt, lt
133             , eq]
134
135 orderBNumOp         =  [[(mul,  Mul),
136                          (div', Div),
137                          (mod', Mod)],
138                         [(add, Add),
139                          (sub, Sub)]]
140
141 boolLit             = [(true, True), (false, False)]
142 uBoolOp             = [(not', Not)]
143 binaryBoolOp        = [(and', And), (or', Or)]
144 binaryAltBoolOp     = [(gt, GreaterThan), (lt, SmallerThan), (eq, Equals)]
145
146 -- erros
147 noParse             = "No parse was found!!!!!!"
148 ambiguousParse      = "Parse is ambiguous!!!!!!"
149 evalerror           = "something went wrong when evaluating"
150 varNotFound         = "var was not found"
151 impossibleState     = "state not possible"
