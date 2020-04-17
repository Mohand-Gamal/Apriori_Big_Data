import pandas as pd
from numpy import random
import matplotlib.pyplot as pyplot
import sys #only needed to determine pythin version number
import matplotlib #only needed to determine Matplotlib version number number

def GetInput(words):
    while True:
        try:
            dataIn=float(input(words+" :\n"))     
        except (ValueError):
            print("Not a number! Try again.")
            continue
        if((dataIn>100) or (dataIn<0)):
            print("Not in range! Try again.")
            continue
        else:
            break
        return dataIn

def DataReady():
    Location = r'Tic2000\ticdata2000.txt'
    originalData = pd.read_csv(Location, header=None, delimiter="\t", index_col=False)
    #print(originalData.info())
    splitData=originalData[originalData.columns[10:22]]
    splitData.columns = ["MRELSA", "MRELOV", "MFALLEEN", "MFGEKIND",
                         "MFWEKIND", "MOPLHOOG", "MOPLMIDD", "MOPLLAAG",
                         "MBERHOOG", "MBERZELF", "MBERBOER", "MBERMIDD"]
    #print(splitData.info())
    return splitData

def GetSupport(data,minSupport):
    return 0

def GetConfidence(dataSupported,minConfidence):
    return 0

def GetLift(dataConfident):
    return 0

def GetLeverage(dataConfident):
    return 0

def PrintData(dataConfident,dataLift,dataLeverage):
    pass

def main():
    #print(' Python version '+sys.version)
    #print(' Pandas version '+pd.__version__)
    #print(' Matplotlib version '+matplotlib.__version__)
    minSupport=GetInput("Please provide minimum support between 0% and 100%")
    minConfidence=GetInput("Please provide minimum confidence between 0% and 100%")
    data=DataReady()
    #print(data)
    dataSupported=GetSupport(data,minSupport)
    dataConfident=GetConfidence(dataSupported,minConfidence)
    dataLift=GetLift(dataConfident)
    dataLeverage=GetLeverage(dataConfident)
    PrintData(dataConfident,dataLift,dataLeverage)

if __name__ == "__main__":
    main()