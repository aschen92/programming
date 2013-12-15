## EmpiricalAnalysis.py
## Aaron Schendel
## 4/9/12


# Using sorts completed for projects of previous months.

from QuickSort import QuickSort    # nlogn
from MergeSort import MergeSort    # nlogn
from SelectionSort import SelSort  # n**2
from ShellsSort import ShellsSort  # nlogn

from random import random
from time import time
from math import *


def main():

    # Sets up display titles.                                                                                
    print("\n{0:^6}{1:^6}{2:^20}{3:^12}{4:^9}{5:^15}{6:^16}{7:^6}{8:^15}{9:^10}"
          .format("N","|","Quick","Quick/nlogn","|","Merge","Merge/nlogn","|","Shell's","Shell's/nlogn"))
    print("-"*119)


    # Runs through the O(n*log(n)) sorts with incrementing array sizes.
    for j in range(10000,20001,2000):

        # Initializes an array of random integers.
        orig = []
        for i in range(j):
            orig.append(random())


        # Computing the time quicksort took and then dividing that by its hypothesized efficiency.    
        qtime1 = time()
        QuickSort(orig[:],-2,0,0)
        qtime2 = time()
        quickTime = qtime2 - qtime1
        quickTimeLog = quickTime / (log(j) * j)


        # Computing the time merge sort took and then dividing that by its hypothesized efficiency.
        mtime1 = time()
        MergeSort(orig[:],0)
        mtime2 = time()
        mergeTime = mtime2 - mtime1
        mergeTimeLog = mergeTime / (log(j) * j)


        # Computing the time Shell's sort took and then dividing that by its hypothesized efficiency.
        shell1 = time()
        ShellsSort(orig[:])
        shell2 = time()
        shellTime = shell2 - shell1
        shellTimeLog = shellTime / (log(j) * j)

        print("\n{0:^6}{1:^6}{2:^17}{3:^10}{4:^6}{5:^15}{6:^10}{7:^6}{8:^15}{9:^10}".format(j,"|",quickTime,quickTimeLog,"|",mergeTime,mergeTimeLog,"|",shellTime,shellTimeLog))



        
    # For display purposes.
    for i in range(5):
        print()


        
        
    # Sets up display titles.     
    print("\n{0:^6}{1:^6}{2:^15}{3:^18}".format("N","|","Selection","Selection/n**2"))
    print("-"*45)


    # Runs through selection sort, O(n**2), with incrementing array sizes.
    for k in range(1000,2001,200):


        # Initializes an array of random integers.
        orig1 = []
        for i in range(k):
            orig1.append(random())


        # Computing the time the sort took and then dividing that by its hypothesized efficiency.
        seltime1 = time()
        SelSort(orig1[:])
        seltime2 = time()
        SelectionTime = seltime2 - seltime1
        SelectionTimeLog = SelectionTime / (k**2)

        # Displaying the results.
        print("\n{0:^6}{1:^6}{2:^15}{3:^9}".format(k,"|",SelectionTime,SelectionTimeLog))

    print()


main()
