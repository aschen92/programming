## Aaron Schendel
## 2/26/12


def QuickSort(list,l,r):
    
    
    if l < r:
        s = Partition(list,l,r)
        QuickSort(list,l,s-1)
        QuickSort(list,s+1,r)

    return

def Partition(list,left,right):

    i = left
    j = right  + 1
    pivot = list[left]
    while True:
        while True:
            i = i + 1
            if i > right or list[i] >= pivot:
                break
        while True:
            j = j - 1
            if list[j] <= pivot:
                break
        if i < j:
            (list[i], list[j]) = (list[j], list[i])
        else:
            break
    (list[left], list[j]) = (list[j], list[left])
    return j
    
        





def main():
    x = [3,5,1,3,7,7,3,4,5,854,6,7,54,6432,2,4,65432,62345,25,43,6734]
    #x = [2,3,1,6,1,4,5,9]
    QuickSort(x,0,len(x)-1)
    print(x)

main()
