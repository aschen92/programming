## Horspool.py
## Aaron Schendel
## 2/28/12

def Horspool(P,T):

    # Initializing Variables
    Table = ShiftTable(P)
    m = len(P)
    n = len(T)
    i = m - 1

    # Core Horspool Algorithm
    while i <= n-1:
        k = 0
        
        while k <= m-1 and P[m-1-k] == T[i-k]:
            k += 1
            
        if k == m:
            return i - m + 1, Table
        
        else:
            i = i + Table[ord(T[i])]
            
    return -1, Table




def ShiftTable(P):

    # Initialize Variables
    Table = []
    m = len(P)

    # Create a Table with 256 spots to encompass the whole ASCII code.
    for i in range(256):
        Table.append(m)
        
    for j in range(m-1):
        Table[ord(P[j])] = m - 1 - j

        
    return Table

def main():
    # Gets required inputs from user.
    print("\nThis program uses Horspool's algorithm to find the index of the\nfirst occurence of a given phrase in a given text.")
    fileName = input("\nPlease enter filename of text to search: ")
    P = input("Please enter phrase you would like to search for: ")

    # Opens the file they specified and then reads the text to be searched.
    infile = open(str(fileName) + ".txt", "r")
    T = infile.read()

    # Calls the Horspool function.
    index, Table = Horspool(P,T)

    # Displays results to the user.
    if index == -1:
        print('\nSorry, "{0}" does not occur in this text.\n'.format(P))
    else:
        print('\n{0} starts at character {1} in the input text.\n'.format(P,index))
        print('\nThe shift table for "{0}" is as follows: \n'.format(P))
        print(Table)
        print()

        
main()
