#Jordan and Chris
#Takes only numbers as entries
#file must take numbers one per line

def partition(list, start, end):
    pivot = list[end]                          # Partition around the last value
    bottom = start-1                           # Start outside the area to be partitioned
    top = end                                  

    done = 0
    while not done:                            

        while not done:                        
            bottom = bottom+1                  # move the bottom up.

            if bottom == top:                  # If we hit the top.
                done = 1                       # we're done.
                break

            if list[bottom] > pivot:           # bottom out of place
                list[top] = list[bottom]       # put on top
                break                          # search from the top.

        while not done:                        # Until we find an out of place element.
            top = top-1                        # move the top down.
            
            if top == bottom:                  # hit the bottom.
                done = 1                       # done.
                break

            if list[top] < pivot:              # Opposite of bottom being out of place
                list[bottom] = list[top]       # switch
                break                          # 

    list[top] = pivot                          # place pivot
    return top                                 


def quicksort(myList, start, end):
    if start < end:                            
        split = partition(myList, start, end)    
        quicksort(myList, start, split-1)        #sort both halves.
        quicksort(myList, split+1, end)

    return myList

def intro():
    
    print("This program will sort a list using quicksort.\n")
    
    myList = []
    repeat = True
    while repeat:
        
        answer = input("Would you like to enter a list by (K)eyboard or (F)ilename?: ")
        
        if answer.lower() == "k":
            
            goAgain = True
            while goAgain:
              element = input("Enter an element to add to the list: ")
              myList.append(int(element))
              goAgain = input("Enter another element? (y/n): ")
              
              if goAgain.lower() == "n":
                  goAgain = False
              elif goAgain.lower() != "y":
                  print("Enter 'y' or 'n'")
                  
            repeat = False
                  
        elif answer.lower() == "f":
            fileName = input("Enter a file name to load into the list to be sorted: ")
            file = open(fileName)
            data = file.readlines()
            
            for element in data:
                element = int(element.split('\n')[0]) #takes the \n off of each element
                myList.append(element)
            repeat = False
            
        else:
            print("Error. Enter 'k' or 'f'.")
            
    return myList

    
if __name__=="__main__":
    
    unsortedList = intro()

    start = 0
    end = len(unsortedList)-1
    newList = quicksort(unsortedList,start,end)                  # Sort the entire list of arguments

    print("Sorted List: ",newList)          # Print out the sorted list
