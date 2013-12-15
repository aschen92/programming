## Aaron Schendel
## 2/26/12

from random import random


def QuickSort(lst,l,r,comparisonCount):
    temp = []
    
    

    # If the input is of type int as opposed to a
    # list then this will append n random numbers to
    # lst to be sorted.
    if type(lst) is int:
        for i in range(lst):
            temp.append(random())

    # Sets the variables.
        lst = temp
        l = 0
        r = len(lst) - 1

    # If l is -2 it sets only the l and r. -2 was chosen because
    # it will never be the value of l except when it is called
    # from main().
    elif l == -2:
        l = 0
        r = len(lst) - 1

    # QuickSort Alg.
    if l < r:
        s, comps = Partition(lst,l,r)
        comparisonCount = comparisonCount + comps
        QuickSort(lst,l,s-1,comparisonCount)
        QuickSort(lst,s+1,r,comparisonCount)
        

    return comparisonCount

        

def Partition(A,l,r):
    comps = 0
    
    pivot = A[l]
    i = l
    j = r + 1

    while True:
        # Keeps track of comparisons.
        comps += 1
        while True:
            i = i + 1
            if i >r or A[i] >= pivot:
                break

        while True:
            j = j - 1
            if A[j] <= pivot:
                break
            
        if i < j:
            A[i], A[j] = A[j], A[i]

        else:
            break
        
    A[l], A[j] = A[j], A[l]

    return j, comps
    
        





def main():

    print("\nThis program QuickSorts a list of input numbers in a file or\nn random numbers and reports the time and number of comparisons.\n")

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
    comps = QuickSort(n,-2,0,0)

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
