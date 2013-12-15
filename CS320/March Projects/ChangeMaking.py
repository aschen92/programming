## ChangeMaking.py
## Aaron Schendel
## 3/24/12
## Pg. 287

def ChangeMaking(D,n):

    # Initializes the table to the size of n.
    F = [0] * (n + 1)


    # Core ChangeMaking Algorithm
    m = len(D) - 1
    
    for i in range(1,n+1):
        temp = float('infinity')
        j = 0

        while j <= m and i >= D[j]:
            temp = min(F[i-D[j]], temp)
            j += 1
    
        F[i] = temp + 1

    return F[n]


if __name__ == '__main__':

    print("\nThis program follows the Dynamic Programming Change Making Algorithm to \ndetermine the minimum amount of coins needed to add up to a given total. \nAllows user to input available coin denominations.\n")
    print("Note: The coin denomination with a value of 1 is pre-included in this program.\n\n")


    # Initializes coinLst with a leading value 1 because
    # without a coin denomination of 1 many values are impossible
    # to obtain.
    coinLst = [1]


    # Gets input from the user.
    n = eval(input("How many different coin denominations, not including value 1, do you wish to use? "))

    print()


    # Gets coin denominations from the user.
    for i in range(n):
        temp = eval(input("Enter coin denomination: "))
        coinLst.append(temp)


    # Gets total value from the user.
    coinNum = eval(input("\nWhat is the total value you want? "))


    # Runs thefunction to obtain the answer.
    ans = ChangeMaking(coinLst,coinNum)



    print("\nMinimum amount of coins needed to add up to {0} is {1}.\n".format(coinNum,ans))
