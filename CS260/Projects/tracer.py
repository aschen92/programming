# tracer.py
# by John Zelle

#   Version 3 of the Ray Tracer includes moveable camera and Blinn-Phong
#     shading with sphere, triangles, and quads.

from __future__ import division, print_function

import time
import sys

from util import *
from models import *
from image import *
from trans3 import *

#----------------------------------------------------------------------

class Scene:

    def __init__(self):
        self.camera = Camera()
        self.surface = Group()
        self.setBackground((0,0,0))
        self.setLight((100,100,20), (1,1,1))
        self.setAmbient((.2,.2,.2))

    def add(self, surface):
        self.surface.add(surface)

    def setBackground(self, rgb):
        self.background = RGB(*rgb)

    def setLight(self, pos, intensity):
        self.light = Light(Vector3(*pos),RGB(*intensity))

    def setAmbient(self, amb):
        self.ambient = RGB(*amb)

    def setEyeDistance(self, d):
        self.camera.setEyeDistance(d)

    def setWindowSize(self, width, height):
        self.camera.setWindowSize(width,height)

    def setView(self, eye, lookat, up):
        self.camera.setView(eye, lookat, up)

    def rayTrace(self, img, updateFn=None):
        width, height = img.size
        camera = self.camera
        camera.setResolution(width, height)
        #print(self.dx, self.dy)
        info = HitInfo()
        interval = Interval()
        for j in range(height):
            for i in range(width):
                ray = camera.ijRay(i, j)
                interval.set(0, float("inf"))
                #print(ray)
                if self.surface.intersect(ray, interval, info):
                    color = self.blinn_phong(camera, info)
                    #print(color)
                else:
                    color = self.background
                img.setPixel((i, j), color.quantize(255))
            if updateFn:
                updateFn()


    def blinn_phong(self, camera, hinfo):

        # start with ambient
        mat = hinfo.color
        color = self.ambient*mat.ambient
    
        # add Lambertian (diffuse color)
        n = hinfo.normal
        p = hinfo.point
        lpos = self.light.position
        I = self.light.intensity
        lvec = (lpos-p)
        lvec.normalize()
        lambert = max(0,lvec.dot(n))
        color += I*mat.diffuse.scalarMult(lambert)

        # add specular highlight
        vvec = camera.eye - hinfo.point
        vvec.normalize()
        hvec = (vvec+lvec)
        hvec.normalize()
        spec = max(0,hvec.dot(n))**mat.exponent
        color += I*mat.specular.scalarMult(spec)
        return color
        

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
    scene.add(Sphere(pos = (0,0,-5), radius=2, color=Material((.8,.3, .3))))
    scene.setBackground((1,1,1))
    scene.setEyeDistance(10)
    scene.setWindowSize(10,10)
    scene.setLight((100, 100, 50), (1,1,1))
    scene.setAmbient((.4,.4,.4))
    scene.setView((0,0,10), (0,0,0), (0,1,0))
    img = Image(resolution, resolution)
    t1 = time.time()
    scene.rayTrace(img)
    #scene.rayTrace(img, Progress(img.size[1]).show)
    #scene.rayTrace(img, Movie(img).show)
    t2 = time.time()
    img.savePPM(fname)
    return t2-t1

if __name__ == "__main__":
    import sys
    res = sys.argv[1]
    outfile = "images/test2-"+res+".ppm"
    t = test(int(res), outfile)
    print("render time =",t)

