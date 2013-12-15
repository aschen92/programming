## RobotCoin.py
## Aaron Schendel
## 3/24/12
## Pg. 289


def RobotCoin(C):

    # Initializing variables.
    n = len(C)
    m = len(C[1])
    
    # Creating the matrix.
    F = [[0 for j in range(m+1)] for i in range(n+1)]
    
    F[1][1] = C[1][1]

    # Core RobotCoin Algorithm.
    for j in range(2,m):
        F[1][j] = F[1][j-1] + C[1][j]

    for i in range(2,n):
        F[i][1] = F[i-1][1] + C[i][1]

        for j in range(2,m):
            F[i][j] = max(F[i-1][j], F[i][j-1]) + C[i][j]
            

    return F[n-1][m-1]


if __name__ == '__main__':

    print("\nThis program solves the Robot Coin Collection problem \nusing a Dynamic Programming algorithm.\n\n")


    # Initializing.
    cList = []
    

    # Gets matrix dimensions from the user.
    n = input("Please enter the number of rows in the matrix: ")
    n = eval(n)
    m = input("Please enter the number of columns in the matrix: ")
    m = eval(m)
    

    # Creates the users matrix from those dimensions.
    C = [[0 for j in range(m+1)] for i in range(n+1)]
    

    # Allows the user to input coordinates where coins
    # are located.
    z = input("\nEnter a coordinate where a coin is in the format x,y: ")
    while z != "":
        x = z.split(",")
        x[0],x[1] = eval(x[0]),eval(x[1])
        cList.append(x)
        z = input("Enter a coordinate where a coin is. Use the format x,y <Enter to solve>: ")

    # Places coins in the user's matrix.
    for [x,y] in cList:
        C[x][y] = 1

        
        
    # Runs the Robot Coin function to obtain the answer.
    ans = RobotCoin(C)


    
    print("\n\nThe maximum number of coins that can be obtained is {0}.\n".format(ans))

    

