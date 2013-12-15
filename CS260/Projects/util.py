# util.py
# by John Zelle

# Helper Classes for simple ray tracer

from __future__ import division, print_function

from vec3 import *

#----------------------------------------------------------------------    
class Interval:

    def __init__(self, min=0.0, max=float("inf")):
        self.min = min
        self.max = max

    def set(self, min, max):
        self.min = min
        self.max = max

    def __contains__(self, t):
        return self.min < t < self.max
    
    def __repr__(self):
        return "Interval({0},{1})".format(self.min, self.max)

#----------------------------------------------------------------------
class Ray: 

    def __init__(self, start, dir):
        self.start = start
        self.dir = dir

    def pointAt(self, t):
        return self.start + t*self.dir

    def __repr__(self):
        return "Ray({0},{1})".format(self.start, self.dir)

#----------------------------------------------------------------------
class RGB:

    def __init__(self, r,g,b):
        self.r = r
        self.b = b
        self.g = g

    def quantize(self, top):
        return (min(int(self.r*top+.5), 255),
                min(int(self.g*top+.5), 255),
                min(int(self.b*top+.5), 255))

    def scalarMult(self, i):
        return RGB(i*self.r, i*self.g, i*self.b)

    def __add__(self,other):
        return RGB(self.r+other.r, self.g+other.g, self.b+other.b)

    def __mul__(self,other):
        return RGB(self.r*other.r, self.g*other.g, self.b*other.b)
    
    def __repr__(self):
        return "RGB({0},{1},{2})".format(self.r, self.g, self.b)


#----------------------------------------------------------------------
class HitInfo: #t, point, normal color

    def fill(self, t, pt, normal, color):
        self.t = t
        self.point = pt
        self.normal = normal
        self.color = color

    def __repr__(self):
        return "HitInfo({0}, {1}, {2}, {3})".format(
            self.t, self.color, self.normal, self.point)

#------------------------------------------------------------
class Light:

    def __init__(self, position, intensity):
        self.position = position
        self.intensity = intensity

    def __repr__(self):
        return "Light({0},{1})".format(self.position, self.intensity)

#------------------------------------------------------------
class Camera:
    """
    >>> c = Camera()
    >>> c.setEyeDistance(50)
    >>> c.eyeDistance
    50
    >>> c.setView(eye=(3,5,10), lookat=(0,5,0), up=(0,1,0))
    >>> c.setWindowSize(200,200)
    >>> c.setView(eye=(3,5,10), lookat=(0,5,0), up=(0,1,0))
    >>> c.u
    Vector3(0.957826285221,0.0,-0.287347885566)
    >>> c.v
    Vector3(0.0,1.0,0.0)
    >>> c.w
    Vector3(0.287347885566,-0.0,0.957826285221)
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

    def setEyeDistance(self, d):
        self.eyeDistance = d

    def setView(self, eye, lookat, up):
        self.eye = Vector3(*eye)
        lookv = Vector3(*lookat) - self.eye
        lookv.normalize()
        upv = Vector3(*up)
        self.u = crossProd(lookv, upv)
        self.u.normalize()
        self.v = crossProd(self.u, lookv)
        self.w = -lookv

    def setWindowSize(self, width, height):
        self.window = (-width/2, -height/2, width/2, height/2)

    def setResolution(self, width, height):
        xll, yll, xur, yur = self.window
        self.dx = (xur-xll)/width
        self.dy = (yur-yll)/height

    def ijRay(self, i, j):
        l,b,r,t = self.window
        x = l + (i+.5) * self.dx
        y = b + (j+.5) * self.dy
        return Ray(self.eye, x*self.u+y*self.v-self.eyeDistance*self.w)

def dotProd(a,b):
    return a.dot(b)

def crossProd(a,b):
    return a.cross(b)

