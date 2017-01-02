import os

dirList=os.listdir(".")
for fname in dirList:
    print fname




def createName(filename):
    pass
def addlines(filename,outputname):
    with open(filename,"r") as input:
        with open(outputname,"w") as output:
            linenumber = 0
            for line in input:
                
                linenumber += 1
