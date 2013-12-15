# tracer2.py

from __future__ import division, print_function

import sys
from math import sqrt

from vec3 import *
from util import *
from models import *
from image import *
import time

#----------------------------------------------------------------------
class Scene:

    def __init__(self):
        self.eye = Vector3(0,0,10)
        self.surface = Group()
        self.background = RGB(0,0,0)
        self.window = (-5,-5,5,5)
        self.light = (Vector3(0,0,10), RGB(1,1,1))
        self.ambient =(.4,.4,.4)

    def add(self, surface):
        self.surface.add(surface)

    def setBackground(self, color):
        self.background = RGB(*color)

    def setEyeDistance(self, d):
        self.eye = Vector3(0,0,d)

    def setWindowSize(self, width, height):
        dx = width/2
        dy = height/2
        
        self.window = (-dx,-dy,dx,dy) 
    
    def setLight(self, pos, intensity):
        self.light = (Vector3(*pos), RGB(*intensity))

    def setAmbient(self,intensity):
        self.ambient = RGB(*intensity)


    def rayTrace(self, img, updateFn=None):
        img.clear(self.background.quantize(255))

        l,b,r,t = self.window
        #print(l,b,r,t)
        w,h = img.size
        dx = (r-l)/w
        dy = (t-b)/h
        hitRec = HitInfo()
        for i in range(w):
            for j in range(h):
                x = l + (i+.5)*dx
                y = b + (j+.5)*dy
                
                r = Ray(self.eye, Vector3(l+(i+.5)*dx,b+(j+.5) *dy,-self.eye.z))
                
                

                if self.surface.intersect(r,Interval(),hitRec):
                    print(self.light)
                    Kd = hitRec.color.diffuse
                    n = hitRec.normal.normalized()
                    lvec = (self.light[0] - hitRec.point).normalized()
                    lambert = Kd.scalarMult(max(0,n.dot(lvec)))

                    ambient = hitRec.color.ambient * self.ambient


                    eyevec = (self.eye - hitRec.point).normalized()
                    specular = hitRec.color.specular.scalarMult( (max(0, n.dot((eyevec + lvec).normalized())))** hitRec.color.exponent)

                    color = lambert + ambient + specular
                    img.setPixel((i,j),color.quantize(255))

                else:
                    img.setPixel((i,j),self.background.quantize(255))
         
                    
        
                
#----------------------------------------------------------------------
class Progress:

    def __init__(self, size):
        self.size=size
        self.count = 0

    def show(self):
        self.count += 1
        print(str(round(self.count/self.size*100,1))+"%")

#----------------------------------------------------------------------
def test(resolution, fname):
    scene = Scene()
    scene.add(Sphere(pos = (0,0,-5), radius=2, color=(0,1,0)))
    scene.setBackground((1,1,1))
    scene.setEyeDistance(10)
    scene.setWindowSize(10,10)
    img = Image(resolution, resolution)
    t1 = time.time()
    scene.rayTrace(img)
    #scene.rayTrace(img, Progress(img.size[1]).show)
    # scene.rayTrace(img, Movie(img).show)
    t2 = time.time()
    img.savePPM(fname)
    return t2-t1

if __name__ == "__main__":
    import sys
    res = sys.argv[1]
    outfile = "test0-"+res+".ppm"
    t = test(int(res), outfile)
    print("render time =",t)

    


