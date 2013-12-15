#lineart.py
from __future__ import division, print_function

from canvas2 import *

def main():
    infile = open("drawing.dat")
    total = int(infile.readline())
    strokes = []
    for i in range(total):
        length = int(infile.readline())
        points = []
        for j in range(length):
            x,y = infile.readline().split()
            x = int(x)
            y = int(y)
            points.append((x,y))
        strokes.append(points)

        
    def drawDino(c,strokes):
        for s in strokes:
            c.drawLines(*s)
        
    c = Canvas(640,480)
    c.scale(.5,.25)
    drawDino(c,strokes)
    

    c.setCoords(0,0,640,480)
    c.translate(300,240)
    c.scale(.2,.5)
    c.refY()
    c.translate(-320,-240)
    drawDino(c,strokes)

    c.setCoords(0,0,640,480)
    c.translate(400,410)
    c.rotate(23)
    c.scale(.15,.25)
    c.translate(-320,-240)
    drawDino(c,strokes)

    c.setCoords(0,0,640,480)
    c.translate(500,180)
    c.scale(.2,.2)
    c.refX()
    c.translate(-320,-240)
    drawDino(c,strokes)

    c.setCoords(0,0,640,480)
    c.translate(180,350)
    c.rotate(43)
    c.scale(.4,.4)
    c.translate(-320,-240)
    drawDino(c,strokes)


    c.getImage().savePPM("drawing.ppm")

main()
