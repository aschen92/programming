#tictactoe.py

# drawing tic tac toe with only a unit square and circle with transformations

from __future__ import division, print_function

from canvas2 import *

def unitSquare(c):
    c.fillPolygon((-.5, -.5), (.5, -.5), (.5,.5), (-.5, .5))

def unitCircle(c):
    c.fillCircle((0,0), 1)

def line(c, length, width, rotate, pos):
    c.saveTransform()

    c.translate(*pos)
    c.rotate(rotate)
    c.scale(length,width)
    unitSquare(c)

    c.restoreTransform()

def ex(c, length, width, pos):
    line(c, length, width, 45, pos)
    line(c, length, width, -45, pos)

def oh(c, radius, thickness, pos):
    c.saveTransform()
    c.translate(*pos)
    c.saveTransform()
    c.scale(radius, radius)
    unitCircle(c)
    c.restoreTransform()
    saveColor = c.color
    c.setColor(c.background)
    c.scale(radius-thickness, radius-thickness)
    unitCircle(c)
    c.setColor(saveColor)
    c.restoreTransform()

def main():
    c = Canvas(400,400)
    c.setCoords(0,0,3,3)
    width = .1
    line(c, 3, width, 0, (1.5,1))
    line(c, 3, width, 0, (1.5,2))
    line(c, 3, width, 90,(1,1.5))
    line(c, 3, width, 90,(2,1.5))

    # Cat's game.
    ex(c, .75, .1, (1.5,1.5))
    ex(c, .75, .1, (1.5,.5))
    ex(c, .75, .1, (.5, 1.5))
    ex(c, .75, .1, (2.5,2.5))
    oh(c, .4, .1, (.5, 2.5))
    oh(c, .4, .1, (.5,.5))
    oh(c, .4, .1, (1.5,2.5))
    oh(c, .4, .1, (2.5,.5))
    oh(c, .4, .1, (2.5,1.5))

    c.getImage().savePPM("ttt.ppm")

main()
    
