## Aaron Schendel
## 2/26/12


def QuickSort(lst,l,r):
    
    
    if l < r:
        s = Partition(lst,l,r)
        QuickSort(lst,l,s-1)
        QuickSort(lst,s+1,r)

def Partition(A,l,r):

    print(A[l])
    pivot = A[l]
    i = l - 1
    j = r  + 1

    while True:

        if i >= j:
            break

        while True:
            i = i + 1
            if A[i] >= pivot:
                break

        while True:
            j = j - 1
            if A[j] <= pivot:
                break

        A[i], A[j] = A[j], A[i]

    A[i], A[j] = A[j], A[i]
    A[l], A[j] = A[j], A[l]

    return j
    
        





def main():
    #x = [3,5,1,6,7,54,6432,2,4,65432,62345,25,43,6734]
    x = [2,3,1,6,1,4,5,9]
    QuickSort(x,0,len(x)-1)
    print(x)

main()
