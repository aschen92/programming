from image import *
from canvas import *



def dat_reader(fname,outputname):
    
    
    verticeslist = []
    vertices = []
    xlargest = 0
    ylargest = 0

    infile = open(fname, "rb")


    strokeNum = infile.readline()
    strokeNum = int(strokeNum)

    for i in range(strokeNum):
        points = infile.readline()
        points = int(points)
        
        for j in range(points):
            temp = infile.readline()
            coords = temp.split()
            x,y = coords[0], coords[1]
            x,y = int(x),int(y)

            if x > xlargest:
                xlargest = x

            if y > ylargest:
                ylargest = y

            vertices.append((x,y))

            
        verticeslist.append(vertices)
        vertices = []


 
    c = Canvas(xlargest+5,ylargest+5)

    for i in range(len(verticeslist)):
        vertices = verticeslist[i]
        c.drawLines(*vertices)

    c.getImage().savePPM(outputname)

    
def main():
    
    fname = input("Enter name of the .dat file: ")
    outputname = input("Enter name of desired .ppm output file: ")
    dat_reader(str(fname) + ".dat",str(outputname) + ".ppm")
    

main()


