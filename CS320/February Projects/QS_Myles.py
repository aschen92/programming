def QuickSort(lst,l,r):
    
    
    if l < r:
        s = Partition(lst,l,r)
        QuickSort(lst,l,s-1)
        QuickSort(lst,s+1,r)

def Partition(A,l,r):

    
    pivot = A[l]
    i = l
    j = r  

    while i < j:
        while A[i] < pivot:
            i = i + 1
        while A[j] > pivot:
            j = j + 1

    A[i], A[j] = A[j], A[i]
    A[l], A[j] = A[j], A[l]

    return j
    
        


    return j



def main():
    x = [3,5,1,6,7,54,6432,2,4,65432,62345,25,43,6734]
    #x = [2,3,1,6,1,4,5,9]
    QuickSort(x,0,len(x)-1)
    print(x)

main()
