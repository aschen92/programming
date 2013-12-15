## MFKnapsack.py
## Aaron Schendel
## 3/27/12
## Pg. 295

    
def Knapsack(i,j,Weights,Values,F):

    # Runs only the first time, initializes matrix F.
    if F == -1:
        
        F = [[-1 for x in range(j+1)] for y in range(len(Weights)+1)]

        for l in range(j+1):
            F[0][l] = 0

        for m in range(len(Weights)+1):
            F[m][0] = 0


    #-------------------------------------------------------------------------


    # Base Case
    if j < 0:
        return 0

    # Core Memo Knapsack Algorithm
    if F[i][j] < 0:
        if j < Weights[i]:
            value = Knapsack(i-1,j,Weights,Values,F)

        else:
            value = max(Knapsack(i-1,j,Weights,Values,F),
                        Values[i] + Knapsack(i-1,j-Weights[i],Weights,Values,F))
            

        F[i][j] = value

    return F[i][j]






if __name__ == "__main__":

    print("\nThis program follows the Knapsack Algorithm to find the maximal value of \nitems that a knapsack can contain with a specified weight limit.\n\n")


    # Initializing the Weights and Values arrays with a dummy  
    # value of 0 to fix indexing issues.
    Weights = [0]
    Values = [0]

    
    i = eval(input("What is the total number of items? "))
    j = eval(input("What is the maximum weight that the knapsack can hold? "))
    print()


    # Obtains the inputs for the problem.
    for x in range(1,i+1):
        tempa = eval(input("Enter the weight of item {0}: ".format(x)))
        Weights.append(tempa)

        tempb = eval(input("Enter the value of item {0}: ".format(x)))
        Values.append(tempb)

        print()


    # Runs the actual function to obtain the answer.
    ans = Knapsack(i,j,Weights,Values,-1)


    print("\nThe maximum value that your knapsack can contain with the given inputs is {0}.\n".format(ans))
