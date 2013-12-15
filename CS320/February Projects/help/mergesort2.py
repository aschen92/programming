# Jordan Hochstetler and chris smith
# 2/10/2012

def merge(left, right):#work on merge
    result = []
    i ,j = 0, 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i = i + 1
        else:
            result.append(right[j])
            j = j + 1
            
    result = result + left[i:]
    result =  result + right[j:]
    return result
            
def mergesort(myList):
    print(myList)
    if len(myList) < 2:
        return myList
    else:
        middle = len(myList) // 2 #keep splitting the list in half
        left = mergesort(myList[:middle]) #do this for the left and right lists
        right = mergesort(myList[middle:])
        return merge(left, right) #put the lists back together
    
def intro():
    
    print("This program will sort a list using merge sort.\n")
    
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

def main():

    l = intro()
    m = mergesort(l)
    print("sorted list: ", m)
main()

    
