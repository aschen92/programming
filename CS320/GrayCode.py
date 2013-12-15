## GrayCode.py
## Aaron Schendel
## 1/27/12

def GrayCode(n):

    # Recursion base case: if n equals one.
    if n == 1:
        L = ['0','1']

    
    else:
        # Recursively calls the function and
        # sets L1 as the list from the previous
        # call.
        L1 = GrayCode(n-1)

        # Puts the reverse of L1 into L2.
        L2 = []
        for i in range(len(L1)):
            L2.append(L1[i])
        L2.reverse()
        
        # Puts a 0 and 1 in front of each
        # bit string in L1 and L2, respectively.
        for i in range(len(L1)):
            L1[i] = "0" + L1[i]
                       
        for i in range(len(L2)):
            L2[i] = "1" + L2[i]


        # Appends L2 to L1.
        for i in range(len(L2)):
            L1.append(L2[i])
            
        L = L1

    
    return L

# Simple main for testing this and
# printing the result.
def main():
    
    x = GrayCode(4+)
    print(x)

main()
