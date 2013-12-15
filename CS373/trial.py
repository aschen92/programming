def max(L):
    ans = L[0]
    for i in L[1:]:
        if i < ans:
            i = ans

    return ans
