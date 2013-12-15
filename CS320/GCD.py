## GCD.py
## Aaron Schendel
## 01/24/2012

from math import sqrt,floor

#----------------------------------------------------------

# Euclid's Algorithm
def Euclid(n,m):
  # returns the greatest common divisor of n and m.
  # PRE: n and m are positive integers  [terminates one step sooner if n ≥ m]
  while  m > 0:
      n, m = m, n%m

  return n


#-----------------------------------------------------------

# Consecutive Integer Checking
def ConsIntCheck(n,m):
  t = min(m,n)

  if m%t == 0 and n%t == 0:
    print("ans is " +str(t))

  else:
    while n%t != 0 or m%t != 0:
      t = t-1
      
    print("GCD = " +str(t))
        
          
#-----------------------------------------------------------

            
# Middle-School Procedure
def MidSchool(n,m):

  # Get prime lists of size n and m.
  primesN = sieveE(n)
  primesM = sieveE(m)

  # Initialize all variables needed in the function.
  nFinal = n
  mFinal = m
  commonFactorList = []
  factorListN = []
  factorListM = []
  finalMinList = []
  ans = 1

  # Generates factor lists for both n and m.
  for i in range(len(primesN)):
    while n % primesN[i] == 0:
      factorListN.append(primesN[i])
      n = n/primesN[i]
  
  for i in range(len(primesM)):
    while m % primesM[i] == 0:
      factorListM.append(primesM[i])
      m = m/primesM[i]

 
  # Determines the longer and shorter list
  # and sets them to variables for future use.
  if len(factorListN) > len(factorListM):
    maxList = factorListN
    minList = factorListM
  elif len(factorListN) == len(factorListM):
    maxList = factorListN
    minList = factorListM
  else:
    maxList = factorListM
    minList = factorListN

  

 

  # Compares each item in the smaller list to
  # all the items in the larger list and then
  # appends them to a new list to be added.
  for i in range(len(minList)):
    if minList[i] in maxList:
      commonFactorList.append(minList[i])
     # maxList.remove(minList[i])

  # Multiplies the common factors together.
  for i in range(len(commonFactorList)):
    ans = ans * commonFactorList[i]

  

  # Displays the results to the user.
  print("\nThe common factors of " +str(mFinal) + " and " + str(nFinal) + " are " + str(commonFactorList))
  print("\nTherefore, the GCD of these two numbers is " + str(ans)+ ".\n")

         


# Sieve of Eratosthenes
def sieveE(n):
  A = [0,0]

  # Creates a list of consecutive ints 2-n+1.
  for p in range(2,n+1):
    A.append(p)

  # Standard Sieve algorithm given in the book.
  j = 0
  for p in range(2,floor(sqrt(n)+1)):
    if A[p] != 0:
      j = p * p
      
      while j <= n:

        A[j] = 0
        j = j + p

      
  L = []
  for i in range(n+1):
    L.append(i)
    
  i = 0
  for p in range(2,n+1):
    if A[p] == 0:
      L[p] = A[p]
      i += 1
  z = L.count(0)

  # Removes 0 placeholders to produce final list.
  while L.count(0) != 0:
    L.remove(0)

  # Strips the "1" off of the beginning.
  L = L[1:]
  return L



