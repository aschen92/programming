def quicksort(list, first, last):
    
    if first < last:
        
        p = partition2(list, first, last)
        quicksort(list, first, p-1)
        quicksort(list, p+1, last)
        
    return

def partition2(list, first, last):
    i = first
    j = last+1 
    pval = list[first]
    while True:
        while True:
            i = i+1
            if i > last or list[i] >= pval:
                break
        while True:
            j = j-1
            if list[j] <= pval:
                break
        if i < j:
            (list[i],list[j]) = (list[j],list[i]) # swap
        else:
            break
    (list[first],list[j]) = (list[j],list[first]) # swap
    return j

def main():
    x = [3,5,1,3,7,7,3,4,5,854,6,7,54,6432,2,4,65432,62345,25,43,6734]
    #x = [2,3,1,6,1,4,5,9]
    quicksort(x,0,len(x)-1)
    print(x)

main()
