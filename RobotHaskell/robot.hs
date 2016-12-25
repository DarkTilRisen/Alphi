import MBot

-- short linefollowing program



a d x=maybe(stop d)($d)$lookup x[(LEFTB,goLeft),(RIGHTB,goRight),(BOTHB,goAhead),(BOTHW,goBackwards)]
r d i =readLineFollower d>>=a d>> print i >>r d (i + 1)
main=openMBot>>= \x -> r x 0
