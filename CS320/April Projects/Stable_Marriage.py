## Stable_Marriage.py
## Aaron Schendel
## 4/11/12
## Pg. 381

## Received help from Brett Peterson on Stable_Marriage function.


def Stable_Marriage(men,women,menMat,womenMat):

    
    # Initializing variables for later use.
    couple = []
    singleMen = men +[]
    singleWomen = women+ []

    # Keeps track of the number of proposals each man has made.
    propList = [1,1,1]

    # Loops until there are no single men left to be paired.
    while len(singleMen) != 0:

        # Sets which man and which woman are being compared.
        man = singleMen[0]
        manVal = men.index(man)
        womanVal = menMat[manVal].index(propList[manVal])
        woman = women[womanVal]

        # Displays each proposal.
        print(man + " proposed to " + woman + ".")

        # If the woman is single put the current man and
        # woman into the couple list because they are married.
        # Also removes each of them from the single list they
        # were in.
        if woman in singleWomen:
            couple.append([man,woman])
            propList[manVal] += 1
            singleMen.remove(man)
            singleWomen.remove(woman)

            print(woman + " accepted.\n")
            
        # If the woman is married this finds her current husband.
        else:
            for pair in couple:
                if woman in pair:
                    tempCouple = pair
                    indexVal = couple.index(pair)

            # If her preference for the new guy is higher she dumps her current husband,
            # the couple is updated, and the old guy is put back in the single list while
            # the new guy is removed from it.
            if womenMat[manVal][womanVal] < womenMat[men.index(pair[0])][womanVal]:
                print(woman + " replaced " + couple[indexVal][0] + " with " + man + ".\n")
                couple[indexVal] = [man,woman]
                singleMen.append(tempCouple[0])

                singleMen.remove(man)
                

               
            # Otherwise he's rejected and just the proposal list is updated.
            else:
                propList[womanVal] += 1
                print(woman + " rejected.\n")

    
    print("{0:^35}".format("Results"))
    print("-" * 40)

    # Loops through each couple and displays their marriage.
    for i in range(len(couple)):
        print("{0} is in a stable marriage with {1}.".format(couple[i][0],couple[i][1]))

    
        
    
    print()
    
    
def main():
    print("\nThis program outputs stable marriages based on given \npreference lists of men and women.\n")


    # Gets the list of men and capitalizes their names.
    m = input("Enter the men's names seperated by commas without spaces: ")
    men = m.split(',')
    for i in range(len(men)):
        men[i] = men[i].title()

    # Gets the list of women and capitalizes their names.
    w = input("Enter the women's names seperated by commas without spaces: ")
    women = w.split(',')
    for j in range(len(women)):
        women[j] = women[j].title()

    # Defensive programming so that the program doesn't break if the
    # user forgets to input a name or inputs too many for either gender.
    while len(men) != len(women):
        print("-" *60)
        print("\nPlease enter the same number of men as women. Thank you!\n")
        m = input("Enter the men's names seperated by commas without spaces: ")
        men = m.split(',')
        for i in range(len(men)):
            men[i] = men[i].title()

        w = input("Enter the women's names seperated by commas without spaces: ")
        women = w.split(',')
        for j in range(len(women)):
            women[j] = women[j].title()
    


    menMat = []
    womenMat = []

    print()

    # Creates the men's ranking matrix based on input from the user.
    for i in range(len(men)):
        menTempMat = []

        for j in range(len(men)):
            temp = eval(input("What is {0}'s preference number for {1}? ".format(men[i],women[j])))
            menTempMat.append(temp)

        menMat.append(menTempMat)
        print()
        
    print()


    # Creates the women's ranking matrix based on input from the user.
    for i in range(len(women)):
        womenTempMat = []

        for j in range(len(women)):
            temp = eval(input("What number is {0} on {1}'s preference list? ".format(men[i],women[j])))
            womenTempMat.append(temp)

        womenMat.append(womenTempMat)
        print()

    
    print()
    

    # Runs the Stable Marriage function.
    Stable_Marriage(men,women,menMat,womenMat)



main()
