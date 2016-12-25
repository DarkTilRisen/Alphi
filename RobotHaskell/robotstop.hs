import MBot

--program to stop the motors of the Mbot
main :: IO ()
main = do {
  d <- openMBot;
  stop d;
  closeMBot d
}
