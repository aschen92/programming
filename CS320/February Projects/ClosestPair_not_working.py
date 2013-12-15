## ClosestPair.py
## Aaron Schendel
## 2/26/2012


from math import ceil, sqrt

def ClosestPair(P,Q):

    x = 0
    y = 1
    n = len(P)

    Pl = []
    Ql = []
    Pr = []
    Qr = []

    dBF = 1e10000
#    current = 1e10000


    if n <= 3:

        for i in range(0,n):
            for j in range(i+1,n):
                dBF = min(dBF, sqrt(((P[i][x] - P[j][x]) ** 2) + ((P[i][y] - P[j][y]) ** 2)))
#                if dBF< current:
#                    P1, P2 = P[i][x]
#                    current = dBF


        return dBF

    else:

        Pl = P[:n//2]
        Ql = Q[:n//2]

        Pr = P[n//2 + 1:n]
        Qr = Q[n//2 + 1:n]


        dL = ClosestPair(Pl, Ql)
        dR = ClosestPair(Pr, Qr)
        d = min(dL,dR)
        m = P[ceil(n/2) - 1][x]
        S = []

        for i in range(0,n):
            if abs(Q[i][x] - m) < d:
                S.append(Q[i])

        num = len(S)

        dminsq = d * d

        for i in range(0, num - 2):
            k = i + 1

            while k <= num - 1 and ((S[k][y] - S[i][x]) ** 2) < dminsq:
                dminsq = min(((S[k][x] - S[i][x]) ** 2) + ((S[k][y] - S[i][y]) ** 2), dminsq)
                k += 1

    return sqrt(dminsq)

def main():
    P = [(1,9),(1.5,9),(2,2),(5,5)]
    Q = [(2,2),(5,5),(1.5,9),(1,9)]




    #P = [(1,7), (2,5), (4,3), (7,12), (9,6)]
    #Q = [(4,3), (2,5), (9,6), (1,7), (7,12)]
    ans = ClosestPair(P,Q)
    print(ans)

main()
