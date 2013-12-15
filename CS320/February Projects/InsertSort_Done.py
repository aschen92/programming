## InsertSort.py
## Aaron Schendel
## 2/26/12

from random import random
from time import time

def InsertionSort(n):

    # Initializing Variables.
    v = 0
    lst = []
    j = 0
    comparisonCount = 0


    # If the input is of type int as opposed to a
    # list then this will append n random numbers to
    # lst to be sorted.
    if type(n) is int:
        for i in range(n):
            lst.append(random())



    # Otherwise the input is the list which is
    # stored as lst.
    else:
        lst = n


    # I continually got an error unless I imported
    # time here.
    from time import time

    # Took the first time outside of the core part
    # of the sort algorithm.
    t1 = time()


    # Core Algorithm for Insertion Sort.
    for i in range(len(lst)):
        v = lst[i]
        j = i-1

        while j >= 0 and lst[j] > v:
            lst[j+1] = lst[j]
            j = j-1
            comparisonCount += 1

        lst[j+1] = v

    # Took second time mark.
    t2 = time()

    # Computes the runtime.
    time = t2-t1
    

    return comparisonCount, time


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
    

    # Calls the core comparison function.
    comps, time = InsertionSort(n)
    

    # If ComparisonSort() was called with a list it is
    # changed to an int, the length of the list, for
    # display purposes.
    if type(n) is list:
        n = len(n)
        

    # Displays the results for the user.
    print("{0:^6}{1:^6}{2:^17}{3:^6.6}{4:^10}".format(n,"|",time,"|",comps))
    print("\n{0:^42}\n".format("Done!"))




main()
