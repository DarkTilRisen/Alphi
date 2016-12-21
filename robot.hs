import MBot
a x = maybe stop id $ lookup x [(LEFTB, goLeft),(RIGHTB, goRight),(BOTHB, goAhead),(BOTHW, goBackwards)]
r d = readLineFollower d >>= flip a d  >> r d
main = openMBot >>= r
