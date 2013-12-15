## LexicographicPermute.py
## Aaron Schendel
## 2/20/2012

from math import factorial

def LexPerm(n):

    # Initializing the first permutation and printing it.
    perm = []
    for x in range(1,n+1):
        perm.append(x)

    print('\n' +str(perm))


    # Initializing i and j.
    i = n-2
    j = n-1
    
    # Running through this loop n!-1 times.
    for k in range(factorial(n)-1):
        
        # Sets i to the largest index in which perm[i] < perm[i+1].
        for i in range(n-2,-1,-1):
            if perm[i] < perm[i+1]:
               break

        # Initializes j and finds the largest index j in which perm[i] < perm[j].
        largestJ=i+1
        for j in range(i+1,n):
            if perm[i] < perm[j]:
                largestJ = j

        j = largestJ

        # Does the last two steps of the algorithm, swaps perm[i] and perm[j]
        # and reverses the list from index i+1 to n.
        perm[i], perm[j] = perm[j], perm[i]
        perm[i+1:] = reversed(perm[i+1:])

        # Displays the current permutation each time around the loop.
        print(perm)
        
    print()
        

# Quick main function that allows user to choose their own n.
def main():
    n = eval(input("Which value would you like to find lexicographical permutations of? "))
    LexPerm(n)

main()
