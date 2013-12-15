## QuickSort.py
## Aaron Schendel
## 2/27/12

def QuickSort(lst,l,r):#,comparisonCount):

    
##    if type(lst) is int:
##        temp = []
##        for i in range(lst):
##            temp.append(random())
##
##        
##        lst = temp
##        l = 0
##        r = len(lst)-1


    
##    # I continually got an error unless I imported
##    # time here.
##    from time import time
##
##    # Took the first time outside of the core part
##    # of the sort algorithm.
##    t1 = time()


    # Recursive Quicksort Algorithm.
    if l < r:
        s = Partition(lst,l,r)#,comparisonCount)
        QuickSort(lst,l,s-1)#,comparisonCount)
        QuickSort(lst,s+1,r)#,comparisonCount)

##    # Took second time mark.
##    t2 = time()
##
##    # Computes the runtime.
##    time = t2-t1
    
    
    #return time



def Partition(A,l,r):#,comparisonCount):
    
    
    p = A[l]
    s = l

    for i in range(l+1,r):
        #comparisonCount += 1
        if A[i] < p:
            s = s + 1
            A[s], A[i] = A[i], A[s]

    A[l], A[s] = A[s], A[l]
    return s #,comparisonCount



def main():
    

    

    x = [3,5,2,1,4,34,5,1,6,7,4,7,52,5]

    
    QuickSort(x,0,len(x)-1)
    print(x)
    

main()
