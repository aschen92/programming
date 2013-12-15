#Jordan Hochstetler and Chris Smith
#python 3
#2/29/2012

def BoyerMooreHorspool(pat, text):
    m = len(pat)
    n = len(text)
    
    if m > n:
        return -1
    
    myList = []
    
    for k in range(256):
        myList.append(m)
        
    for k in range(m - 1):
        myList[ord(pat[k])] = m - k - 1
        
    myList = tuple(myList)
    
    k = m - 1
    
    while k < n:
        j = m - 1; i = k
        while j >= 0 and text[i] == pat[j]:
            j -= 1; i -= 1
        if j == -1: return i + 1
        k += myList[ord(text[k])]
    return -1

def intro():
    
    print("This program will match string with horspool.\n")
        
    fileName = input("Enter a file name to load into the list to be sorted: ")
    file = open(fileName)
    data = file.read()

    return data


if __name__ == '__main__':
    text = intro()
    pattern = input("What would you like to search in the text?: ")
    s = BoyerMooreHorspool(pattern, text)
    print('Text:',text)
    print('Pattern:',pattern)
    if s > -1:
        print('Pattern \"' + pattern + '\" found starting at position',s)
