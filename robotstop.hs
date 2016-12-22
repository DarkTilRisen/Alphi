import MBot



main :: IO ()
main = do {
  d <- openMBot;
  stop d;
  closeMBot d
}
