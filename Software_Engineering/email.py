from __future__ import print_function

class EmailAddress:


    def __init__(self, address):
        self.address = str(address)

    def isID(self, id):

        for char in id:
            if not (ord(char) >= 48 and 57 >= ord(char) or 65 <= ord(char) <= 90 or 97 <= ord(char) <= 122):
                return False

        return True


    def verify(self):
        add = self.address



        if type(add[0]) == int :    #Check that first character in email isn't a number
            return False

        id = add.split("@")
        if self.isID(id[0]) == False:
            
            return False

        if len(id) == 1:        #If we get 1 part, there was no @
            
            return False
        
        elif len(id) > 2:       #If we get more than 2 parts, there was more than one @
            
            return False

        
       
        
        temp = id[1]     
        id2 = temp.split(".")

        
        for currID in id2:       #Make sure each part is length 1, first.last@wartburg.co. is not vaild
            if len(currID)==0:
                
                return False
            elif not self.isID(currID):
                
                return False



        
        return True
                
		

   
                    



