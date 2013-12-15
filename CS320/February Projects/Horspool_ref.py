## Horspool.py
## Aaron Schendel
## 2/28/12

def Horspool(P,T):
    Table = ShiftTable(P)
    m = len(P)
    n = len(T)
    i = m - 1

    while i <= n-1:
        k = 0
        while k <= m-1 and P[m-1-k] == T[i-k]:
            k += 1
        if k == m:
            return i - m + 1, Table
        else:
            i = i + Table[ord(T[i])] 
    return -1, Table




def ShiftTable(P):
    Table = []
    m = len(P)
    for i in range(256):
        Table.append(m)
    # all the characters have a code for each number, make that many spaces
    for j in range(m-1):
        Table[ord(P[j])] = m - 1 - j
        # for the letters in the ord('char') replace them with the right shift 
    return Table

def main():
    P = "it"
    T = 'kitty_cat'

    index, Table = Horspool(P,T) # This returns the index of the first occurance of your pattern
    if index == -1:
        print('Sorry, "{0}" does not occur in this text.'.format(P))
    else:
        print(index)
        print(Table)
main()
