module Main where


import MBot
import Data.Bits

main :: IO ()
main = do {
      d <- openMBot;
      putStrLn $ show $ ( complement 0)
      --goBackwards d;
      --sendCommand d (setMotor 0x9 (complement 0) (complement 60));
      --sendCommand d (setMotor 0xa 60 0);
      closeMBot d;
}
