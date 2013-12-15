# matrix.py
#    A set of routines for operating on matrices, where a matrix
#    is represented as a list of lists.

def matID(n):
    """return n x n identity matrix
    >>> matID(2)
    [[1.0, 0.0], [0.0, 1.0]]
    >>> matID(3)
    [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
    >>> matID(4)
    [[1.0, 0.0, 0.0, 0.0], [0.0, 1.0, 0.0, 0.0], [0.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 1.0]]
    """
    mat = [[0.0]*n for i in range(n)]
    for i in range(n):
        mat[i][i] = 1.0
    return mat


def nrows(mat):
    """ return the number of rows in mat
    >>> m1 = [ [1,2], [3,4] ]
    >>> nrows(m1)
    2
    >>> m2 = [ [1,2], [3,4], [5,6] ]
    >>> nrows(m2)
    3
    """
    return len(mat)


def ncols(mat):
    """ return the number of columns in mat
    >>> m1 = [ [1,2], [3,4] ]
    >>> m2 = [ [1,2], [3,4], [5,6] ]
    >>> ncols(m1)
    2
    >>> ncols(m2)
    2
    >>> m3 = [ [1], [2], [3] ]
    >>> ncols(m3)
    1
    """
    
    return len (mat[0])


def matMult(m1, m2):
    """ return the matrix product of m1 and m2
    >>> m1 = [[1,2],[3,4]]
    >>> m2 = [[-2,3], [1,-1]]
    >>> matMult(m1,m2)
    [[0, 1], [-2, 5]]
    >>> matMult(m2,m1)
    [[7, 8], [-2, -2]]
    >>> matMult(m1,matID(2))
    [[1.0, 2.0], [3.0, 4.0]]
    >>> matMult(matID(2), m2)
    [[-2.0, 3.0], [1.0, -1.0]]
    >>> matMult(m1, [[2], [2]])
    [[6], [14]]
    """

    product = []
    
    for i in range(nrows(m1)):
        row = []

        for j in range(ncols(m2)):
            sum = 0

            for k in range(ncols(m1)):
                sum += m1[i][k] * m2[k][j]
               
            row.append(sum)
        
        product.append(row)

    return product
            
    


def matTranspose(m):
    """ return the transpose of matrix m
    >>> m1 = [[1,2],[3,4]]
    >>> m2 = [[-2,3], [1,-1]]
    >>> matTranspose(m1)
    [[1, 3], [2, 4]]
    >>> matTranspose(m2)
    [[-2, 1], [3, -1]]
    """
    return [list(t) for t in zip(*m)]


def matApply(m, seq):
    """ return the result of applying m to point represented as a sequence
    The sequence in converted to a column vector and the result is
    converted back to a flat list.

    >>> m1 = [[1,2],[3,4]]
    >>> m2 = [[-2,3], [1,-1]]
    >>> matApply(m1, [-1,1])
    [1, 1]
    >>> matApply(m2, [-1,1])
    [5, -2]
    >>> matApply(matID(3), [1,2,3])
    [1.0, 2.0, 3.0]
    """

    
    tempmat = []

    for i in range(len(seq)):
        tempmat.append([seq[i]])

    prod = matMult(m,tempmat)

    ans = []

    for j in range(len(seq)):
        ans.append(prod[j][0])
        
    return ans
    
    
if __name__ == '__main__':
    import doctest
    doctest.testmod()
