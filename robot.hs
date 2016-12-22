import MBot
a d x=maybe(stop d)($d)$lookup x[(LEFTB,goLeft),(RIGHTB,goRight),(BOTHB,goAhead),(BOTHW,goBackwards)]
r d=readLineFollower d>>=a d>>r d
main=openMBot>>=r
