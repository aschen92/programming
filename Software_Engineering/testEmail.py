import unittest
from email import *


class TestEmail(unittest.TestCase):
        

    def test_checkEmails(self):
        infile = open("emailList.txt", "r")
        for line in infile:
            e = EmailAddress(line)
            print(e.address)
            if not self.assertTrue(e.verify()):
                print(e, "is illegal")
            

if __name__ == "__main__":
    unittest.main()
