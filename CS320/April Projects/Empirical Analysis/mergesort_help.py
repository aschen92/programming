# Brett Peterson
#   Mergesort
from random import*


def merge(B,C,A):
    comps = 0
    p = len(B)
    q = len(C)
    i = j = k = 0
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
        A[k:p + q] = C[j:q]
    else:
        A[k:p + q] = B[i:p]
    return comps

def mergesort(A,comps):
    n = len(A)
    if n > 1:
        B = A[0:n//2]
        C = A[n//2:n]
        
        comps = mergesort(B,0)
        comps = mergesort(C,0)

        comps += merge(B,C,A)
    return comps
    

def mergeSort(A):
    from time import time
    t1 = time()
    comps = mergesort(A,0)
    t2 = time()
    t = t2 - t1
    return t,comps

# Gets list of numbers from file
def getFromFile(path):
    infile = open(path,'r')
    numsin = eval(infile.read())
    lst = []
    for num in numsin:
        lst.append(num)
    return lst


def createInput(n):
    lst = []
    for i in range(n):
        lst.append(random())
    return lst

if __name__ == "__main__":
    row = "|{0:^10}|{1:10}|{2:11}|{3:^140}|"
    sep = "+{0:-^10}+{0:-^10}+{0:-^11}+{0:-^140}+".format("")

    print("This is a program to carry out a mergesort.")
    ipt = input("You may enter a filepath to a file containing a list of numbers, or a number (n) to generate n numbers: ")

    print('\n')
    print("+{0:-^174}+".format("Mergesort"))
    print(row.format("n","Time","Comparisons","Sorted Front and Back"))
    print(sep)

    num = eval(ipt)
    if type(num) != str:
        for n in range(10000,eval(ipt) * 10000 + 1,10000):
            lst = createInput(n)
            time,comps = mergeSort(lst)
            
            print(row.format(n,'{0:.5}'.format(time),comps,str(lst[0:3]) + '...' + str(lst[n - 3:n])))
            print(sep)
    else:
        lst = getFromFile(ipt)
        time,comps = mergeSort(lst)
        n = len(lst)
        print(row.format(n,'{0:.5}'.format(time),comps,str(lst[0:3]) + '...' + str(lst[n - 3:n])))
        print(sep)
        
