#!/usr/bin/env python

import os
import sys

def addlines(filename, outputfile):
    with open(filename) as input:
            linenumber = 0
            for line in input:
                outputfile.write(str(linenumber) + " " + line)
                linenumber += 1

if __name__ == '__main__':
    topdir = sys.argv[1]
    exten = sys.argv[3]
    start = True
    with open(sys.argv[2], "w") as output:
        for dirpath, dirnames, files in os.walk(topdir):
            for name in files:
                if name.lower().endswith(exten):
                    output.write(("\n\n" if not start else "") + name+"\n\n" )
                    addlines(os.path.join(dirpath, name),output)
                    start = False
