# camera.py: class to manage view in a ray tracer

from vec3 import *
from util import *

class Camera:
    """
    >>> c = Camera()
    >>> c.setEyeDistance(50)
    >>> c.eyeDistance
    50
    >>> c.setView(eye=(3,5,10), lookat=(0,5,0), up=(0,1,0))
    >>> c.setWindowSize(200,200)
    >>> c.window
    (-100, -100, 100, 100)
    >>> c.setView(eye=(3,5,10), lookat=(0,5,0), up=(0,1,0))
    >>> c.u
    Vector3(0.957826285221,0.0,-0.287347885566)
    >>> c.v
    Vector3(-0.0,1.0,0.0)
    >>> c.w
    Vector3(0.287347885566,0.0,0.957826285221)
    >>> c.setResolution(50,50)
    >>> c.ijRay(0,0)
    Ray(Vector3(3.0,5.0,10.0),Vector3(-108.23437023,-98.0,-19.7312214756))
    >>> c.ijRay(49,49)
    Ray(Vector3(3.0,5.0,10.0),Vector3(79.4995816734,98.0,-76.0514070466))
    >>> c.ijRay(25,25)
    Ray(Vector3(3.0,5.0,10.0),Vector3(-12.4517417079,2.0,-48.4660100322))
    """

    def __init__(self):
        self.eye = Vector3(0, 0, 10)
        self.window = (-10, -10, 10, 10) # left, bottom, right, top
        self.dx = .1                     # horizontal distance between pixels
        self.dy = .1                     # vertical distance between pixels
        self.u = Vector3(1,0,0)       
        self.v = Vector3(0,1,0)
        self.w = Vector3(0,0,-1)
        self.eyeDistance = 10
        self.nx = 10
        self.ny = 10

    def setEyeDistance(self, d):
        self.eyeDistance = d

    def setView(self, eye, lookat, up):

        self.eye = Vector3(*eye)
        self.lookat = Vector3(*lookat)
        self.up = Vector3(*up)

        
        self.w = (self.eye - self.lookat).normalized()
        self.w = self.w
        self.u = self.up.cross(self.w)
        self.v = self.w.cross(self.u)

    def setWindowSize(self, width, height):
        w1 = -width/2
        w2 = width/2
        h1 = -height/2
        h2 = height/2

        self.window = (w1,h1,w2,h2)

    def setResolution(self, width, height):
        l,b,r,t = self.window
        self.dx = (r-l)/width
        self.dy = (t-b)/height

    def ijRay(self, i, j):
        l,b,r,t = self.window
        return Ray(self.eye, self.eyeDistance * -self.w + (l + (i + .5) * self.dx) * self.u + ((b + (j + .5) * self.dy) * self.v))

if __name__ == "__main__":
    import doctest
    doctest.testmod()
