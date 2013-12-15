# Aaron Schendel
# hill_climb.py
#    Simple hill climbing with random restart to solve nqueens

from random import randrange

def random_state(n):
    """ return random n-queen state
    state is a list of (row,col) pairs
    """
    qs = []
    for i in range(n):
        qs.append((randrange(n),i))

    return qs


def countAttacks(qs):
    """ return number of mutual attack pairs in qs
    qs is an n-queen state
    """
    count = 0
    for i in range(len(qs)):
        for j in range(i+1, len(qs)):
            r1,c1 = qs[i]
            r2,c2 = qs[j]
            if r1 == r2 or abs(r1-r2) == abs(c1-c2):
                count += 1
    return count


def neighbors(qs):
    """generate all immediate neighbors of state qs.
    usage: for neighbor in neighbors(state):
               #do domething with neighbor
    """
    n = len(qs)
    for c in range(n):
        qr,qc = qs[c]
        for r in range(n):
            if r != qr:
                neighbor = list(qs)
                neighbor[c] = (r,c)
                yield(neighbor)


def evaluate(qs):
    """ hill-climbing evaluation function
    """
    return -countAttacks(qs)


def hill_climb(currState):
    """hill-climb from currState to find a (local) maximum state
    returns finalState, finalStateEvaluation
    """
    
    localMax = float("-inf")
    

    for n in neighbors(currState):
        temp = evaluate(n)
        if temp > localMax:
            finalStateEvaluation = temp
            finalState = n

   


    return finalState, finalStateEvaluation
    
    
            
    
def solve(n):
    """ solve an n-queen problem using hill_climbing with random restart
    """

    
    qs = random_state(n)
    fs, feval = hill_climb(qs)
    

    while feval != 0:
        qs = random_state(n)
        fs, feval = hill_climb(qs)
        

    return fs
        

    

if __name__ == '__main__':
    import sys, time
    t1 = time.time()
    print(solve(int(sys.argv[1])))
    t2 = time.time()
    print(t2-t1, "seconds")
