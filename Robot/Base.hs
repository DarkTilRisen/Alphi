-- A more intuitive library
module Robot.Base where
import System.HIDAPI hiding (error)
import MBot
import qualified Data.Base as Data
import Data.Bits
import Evaluator.Util
import GHC.Float

motors     = [(MotorR, 0xa), (MotorL, 0x9)]
linesensor = [(SensorL, LEFTB), (SensorR, RIGHTB )]
leds       = [(Led1, 1), (Led2, 2)]
stops      = 0

data Motor       = MotorL  | MotorR  deriving (Eq)
data LineSensor  = SensorL | SensorR deriving (Eq)
data Led         = Led1    | Led2    deriving (Eq)


-- Note I have no clue how this works !!!!!!
-- A more generic and intuitive implementation to control the motors --
-- Speed range [-255,255]
move :: Int -> Motor -> Device -> IO ()
move s m d | m == MotorR && s > 0 = move' s stops
           | m == MotorR          = move' (complement (-s)) (complement stops)
           | m == MotorL && s > 0 = move' (complement s) (complement stops)
           | m == MotorL          = move' (-s) stops
           | otherwise            = error Data.impossibleState
          where move' s x = sendCommand d $ setMotor (find m motors) s x

{-
1 -> Red
2 -> Green
3 -> Blue
-}

led :: Int -> Led -> Device -> IO()
led x l d | x == 0    = f 0   0   0
          | x == 1    = f 100 0   0
          | x == 2    = f 0   100 0
          | x == 3    = f 0   0   100
          | otherwise = error Data.impossibleState
          where f r g b =  sendCommand d $ setRGB (find l leds) b g r

--Read out the ultrasonic sensor and convert it to a IO(Double) --
readUltra :: Device -> IO Double
readUltra d = fmap float2Double (readUltraSonic d)

-- check if the sensor sees white or black --
readLine :: LineSensor -> Device -> IO Bool
readLine s d = readLineFollower d >>= match
            where match LEFTB  = return $ find s linesensor == LEFTB
                  match RIGHTB = return $ find s linesensor == RIGHTB
                  match BOTHB  = return True
                  match BOTHW  = return False
