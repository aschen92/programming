## MergeSort.py
## Aaron Schendel
## 2/27/12

from math import floor
from random import random


def MergeSort(A,comparisonCount):
    # Initializing Variables.
    temp = []
    B = []
    C = []
    


    # If the input is of type int as opposed to a
    # list then this will append n random numbers to
    # lst to be sorted.
    if type(A) is int:
        for i in range(A):
            temp.append(random())

        n = A
        A = temp
        


    # Otherwise set n as the length of A.
    else:
        n = len(A)
        

    # Core MergeSort Algorithm.
    if n > 1:
        B = A[:floor(n/2)]
        C = A[floor(n/2):]

        MergeSort(B,comparisonCount)
        MergeSort(C,comparisonCount)
        comps = Merge(B,C,A)
        comparisonCount = comparisonCount + comps

        
        return comparisonCount


def Merge(B,C,A):
    
    # Initializing Variables.
    comps = 0
    p = len(B)
    q = len(C)
    i = 0
    j = 0
    k = 0
    
    # Core Merge algorithm.
    while i < p and j < q:
        comps += 1
        if B[i] <= C[j]:
            A[k] = B[i]
            i += 1
            
        else:
            A[k] = C[j]
            j += 1
            

        k += 1

    if i == p:
        A[k:p+q] = C[j:q]
    else:
        A[k:p+q] = B[i:p]

    

    return comps
        

def main():
    # Allows user to either input a filename or
    # have the program make n random numbers.
    # This also strips down the input the user says
    # to just the first letter so that any variation
    # of the word "yes" will work.
    choice = input("Would you like get numbers to sort from a file? ")
    choice.upper()
    choice = choice[0]
    

    # Opens file and appends each line to the list n
    # and evals them so that they are ints not strings.
    if choice == "y":
        fileName = input("Please enter the filename: ")
        infile = open(str(fileName) + ".txt", "r")
        n = []
        for line in infile:
            n.append(line)
        for i in range(len(n)):
            n[i] = eval(n[i])
        

    
    else:
        n = eval(input("Enter the amount of random numbers you would like to sort: "))


    # Prints the column headers with a line underneath.
    print("\n{0:^6}{1:^6}{2:^17}{3:^6.5}{4:^10}".format("N","|","Time","|","Comparisons"))
    print("{0:^42}".format("-----------------------------------------------"))
    
    from time import time
    
    # Took the first time outside of the core part
    # of the sort algorithm.
    t1 = time()

    
    # Calls the core comparison function.
    comps = MergeSort(n,0)

    # Took second time mark.
    t2 = time()

    # Computes the runtime.
    time = t2-t1
    

    # If ComparisonSort() was called with a list it is
    # changed to an int, the length of the list, for
    # display purposes.
    if type(n) is list:
        n = len(n)
        

    # Displays the results for the user.
    print("{0:^6}{1:^6}{2:^17}{3:^6.6}{4:^10}".format(n,"|",time,"|",comps))
    print("\n{0:^42}\n".format("Done!"))
    
    
#main()

    
