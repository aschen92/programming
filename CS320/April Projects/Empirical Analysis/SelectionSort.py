## SelectionSort.py
## Aaron Schendel
## 1/29/12

 
def SelSort(array):

    # Outer loop that continually adds one to
    # the sorted part of the list.
    for i in range(len(array)-1):
        minNumIndex = i

        # Inner loop that checks for the next
        # number smaller than the current smallest
        # and then if found sets it to minNumIndex.
        for j in range(i+1, len(array)):
            if array[j] < array[minNumIndex]:
                minNumIndex = j

        # Swaps the positions of the smallest number
        # to the newest position in the sorted portion
        # of the list.
        array[i], array[minNumIndex] = array[minNumIndex], array[i]
            

      
    
    return array

#SelSort([s,h,e,l,l,s,o,r,t,i,s,u,s,e,f,u,l])

