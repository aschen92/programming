## RussianPeasant.py
## Aaron Schendel
## January 26th, 2012

from math import floor

def russianPeasant(n, m):
    # Initializing variables for later use.
    addList = []
    x,y = n,m
    ans = 0
    curr = 0
    prev = 0

    # Prints the headers for the columns with a line going across the bottom.
    print("\n{0:^6}{1:^2}{2:^10}{1:^2}{3:^10}".format("n","|","m","Addition List"))
    print("----------------------------------")

    # Runs as long as the number that is being halved isn't equal to 1.
    while n != 1:
        # If n is odd then we halve it but append it to the
        # list of numbers to be added since it didn't divide
        # evenly.
        if n%2 != 0:
            addList.append(m)
            curr = curr + 1

        # Uses curr and prev temporary variables to
        # determine if m needs to be printed in just
        # the m column or also in the "Addition List"
        # column.
        if curr > prev:
            print("{0:^6}{1:^2}{2:^10}{1:^2}{3:^10}".format(n,"|",m,m))
            prev = prev + 1
        else:
            print("{0:^6}{1:^2}{2:^10}{1:^2}".format(n,"|",m))

        # Halves n (rounding down) and doubles m.
        n = floor(n/2)
        m = m * 2

    # Once n equals one that m will end up in the
    # "Addition List" column as well.
    print("{0:^6}{1:^2}{2:^10}{1:^2}{3:^10}".format(n,"|",m,m))
    addList.append(m)
    

    # Adds up the numbers in the addList which becomes
    # the overall product of the original n and m.
    for i in range(len(addList)):
        ans = ans + addList[i]

    
    
    
    # Displays to the user that the sum of the
    # "Addition List" is the same as the product
    # of n and m.
    print("\nThe sum of " +str(addList) + " is " +str(ans)+".")
    print("\nTherefore, the product of " +str(x) +" and " +str(y) + " is also " +str(ans)+".\n")

        
russianPeasant(50,65)
