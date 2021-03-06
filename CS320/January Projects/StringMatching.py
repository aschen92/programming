## StringMatching.py
## Aaron Schendel
## 1/30/2012

# This function is designed to be case-sensitive.
def StringMatching():

    # Gets required inputs from user.
    fileName = input("Please enter filename of text to search: ")
    p = input("Please enter phrase you would like to search for: ")

    # Opens the file they specified and then reads the text to be searched.
    infile = open(str(fileName) + ".txt", "r")
    t = infile.read()
    

    # Initializing variables to be used later.
    n = len(t)
    m = len(p)
    ans = []


    # Main loop that runs the length of text minus the length of the phrase.
    for i in range(n-m+1):
        j = 0

        # Checks first to see if the letter in the
        # phrase matches up with the letter in the
        # text and then checks the following letters
        # for matches until it reaches the length of
        # the phrase.
        while j < m and p[j] == t[i+j]:
            j += 1

        # If the while loop above goes through then
        # j will equal m and the index of the phrase
        # will be generated by the for loop below,
        # the index will be displayed to the user,
        # and the program will terminate.
        if j == m:
            for a in range(m):
                ans.append(i+a)
                
            print("\n{0} {1} {2}.\n".format("This phrase occurs in indexes",ans,"of the input text"))
            return
            
            
            

    # If the phrase is not found in the text
    # an error message explaining that will
    # be displayed.
    print("\nERROR: Input phrase does not occur in input text file.\n")
    
    


StringMatching()
