## CoinRow.py
## Aaron Schendel
## 3/23/12
## Pg. 285

def CoinRow(C):

    # Initializes F and coinList which is the
    # list containing the coins that were used
    # to obtain the max value.
    F = [0,C[1]]
    coinList = [[],[C[1]]]

    n = len(C)

    # Core Coin Row Algorithm.
    for i in range(2,n):
        F.append(max(C[i] + F[i-2], F[i-1]))
        

        # Code to determine coins used.
        if C[i] + F[i-2] > F[i-1]:
            coinList.append(coinList[i-2][:]+[C[i]])

        else:
            coinList.append(coinList[i-1][:])


    return F[n-1],coinList[n-1]


if __name__ == '__main__':

    print("\nThis program computes the maximal result for the coin-row problem.\n")

    # Initializing C with value 0 to fix indexing.
    C = [0]

    # Sentinal loop that obtains the row of coins from the user.
    temp = input("Input coin value: ")
    while temp != "":
        n = eval(temp)
        C.append(n)
        temp = input("Input next coin value <Enter to end>: ")

    # Runs the Coin Row function to obtain the answer.
    ans,coinsUsed = CoinRow(C)
    

    print("\nThe maximum value you can obtain is {0}.\n".format(ans))
    print("The coins used to obtain {0} are: {1}.\n".format(ans,coinsUsed))
