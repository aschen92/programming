# programming_interviews.py
# Aaron Schendel
# 11-26-13

#-----------------------------------#
# Some programming problems that    #
# are commonly asked in interviews  #
#                                   #
# http://maxnoy.com/interviews.html #
#-----------------------------------#

#Reverses words in a string
def stringRevWords(string):
    words = string.split()
    rev = []
    for word in words:
        rev.insert(0, word)
    revStr = " ".join(rev)
    asn = words.reverse()
    print(asn)
    print(revStr)

#stringRev("I love to eat cats")
#--------------------------------
    
#Reverses a string
def stringRev(string):
    ans = ''
    for i in range(1, len(string)+1):
        ans = ans + string[0-i]
    print(ans)

#stringRev('i love kitties')
#--------------------------------

#Strip whitespace from a string in place
def stripWhite(string):
    ans = string.replace(" ", "")
    print(ans)

#stripWhite("hm i wonder if this works")
#--------------------------------

#Remove duplicate chars from a string
#Note: sets cannot contain duplicates.
def removeDupes(string):
   ans = ' '.join(set(string.split()))
   print(ans)

#removeDupes("a h s s k a h")
#---------------------------------
