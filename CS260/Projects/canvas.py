# canvas.py

#  Class for simple 2D drawing

from __future__ import division, print_function

from math import *
from image import *

X = 0
Y = 1

class Canvas(object):

    def __init__(self, width, height, background=(255,255,255)):
        self.color = (0,0,0)  # default color is black
        self.img = Image(width, height)
        self.img.clear(background)

    def getImage(self):
        """return the image"""

        return self.img



    def setColor(self, rgb):
        """set current drawing color to rgb"""

        self.color = rgb




    def draw(self, p):
        """set pixel nearest p to current color
        NOTE: p is an ordered pair (x,y) where x and y may be floats or ints
        """

        xloc = round(p[X])
        yloc = round(p[Y])
        
        self.img.setPixel((xloc,yloc),self.color)


        
        
    def drawLine(self, a, b):
        """draw line from pixel a to b using current color"""
        if a == b:
            self.draw(a)
            return

        dy = b[Y] - a[Y]
        dx = a[X] - b[X]
        
        if abs(dx) > abs(dy):
            if a[X] > b[X]:
                a,b = b,a
            
            yinc = (b[Y] - a[Y]) / (b[X] - a[X])
            y = a[Y]
            for x in range(int(a[X]),int(b[X]) + 1): #check
                self.draw((x,y))
                y = y + yinc

        else:
            if a[Y] > b[Y]:
                a,b = b,a
            
            xinc = (b[X] - a[X]) / (b[Y] - a[Y])
            x = a[X]
            for y in range(int(a[Y]),int(b[Y]) + 1): #check
                self.draw((x,y))
                x+=xinc



    def drawLines(self, *vertices):
        """draw polyline connecting pixels in vertices in order"""
        for i in range(len(vertices)-1):
            self.drawLine(vertices[i],vertices[i+1])


        

    def drawPolygon(self, *vertices):
        """draw polygon describe by pixels in vertices"""
        self.drawLines(*vertices)
        self.drawLine(vertices[-1],vertices[0])




    def drawCircle(self, center, radius, segments = 50):
        """draw a circle with given center and radius approximated
        by a regular polygon with the given number of segments"""

        vertices = []
        dtheta = (2*pi) / segments
        theta = 0
        
        while theta < (2*pi):
            x = radius * cos(theta) + center[X]
            y = radius * sin(theta) + center[Y]
            vertices.append((x,y))
            theta = theta + dtheta

        self.drawPolygon(*vertices)


    def fillRectangle(self, p0, p1):
        xlow = min(p0[X], p1[X])
        xhigh = max(p0[X], p1[X])
        ylow = min(p0[Y], p1[Y])
        yhigh = max(p0[Y], p1[Y])

        for x in range(xlow,xhigh + 1):
            for y in range(ylow,yhigh + 1):
                self.draw((x,y))


    def fillCircle(self, c, r):
        xlow = c[X] - r
        xhigh = c[X] + r
        ylow = c[Y] - r
        yhigh = c[Y] + r
        
        for x in range(xlow, xhigh + 1):
            for y in range(ylow,yhigh + 1):
                if ((c[X] - x) ** 2) + ((c[Y] - y) ** 2) <= r ** 2:
                    self.draw((x,y))


    def fillTriangle(self, a, b, c):
        def f(p0,p1,x,y):
            x0,y0 = p0
            x1,y1 = p1

            return ((y0-y1) * x) + ((x1-x0) * y) + (x0*y1) - (x1*y0)

        
        xlow = min(a[X],b[X],c[X])
        xhigh = max(a[X],b[X],c[X])
        ylow = min(a[Y],b[Y],c[Y])
        yhigh = max(a[Y],b[Y],c[Y])

        for x in range(xlow,xhigh + 1):
            for y in range(ylow,yhigh + 1):
                alpha = f(b,c,x,y) / f(b,c,a[X],a[Y])
                beta = f(a,c,x,y) / f(a,c,b[X],b[Y])
                gamma = f(a,b,x,y) / f(a,b,c[X],c[Y])

                if alpha >= 0 and beta >= 0 and gamma >= 0:
                    self.draw((x,y))

        

if __name__ == "__main__":
    c = Canvas(400,300)
    c.drawLine((0,0), (200,30))
    c.drawLine((50,100), (60, 250))
    c.drawCircle((200,150), 100, 50)
    c.setColor((0,255,0))
    c.drawPolygon((50,50), (100,50), (100,100), (50,100))
    c.setColor((255,0,0))
    c.fillRectangle((50,260),(65,280))
    c.setColor((0,0,255))
    c.fillCircle((100,260), 25)
    c.setColor((0,255,255))
    c.fillTriangle((20,20),(40,40),(40,10))
    c.getImage().savePPM("canvas_test.ppm")
    
        
