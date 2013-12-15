# test5a.py
#   Draw barn scene with mesh
from __future__ import division, print_function

import sys

from models import *
from tracer import *
from image import *

def main(res):
    scene = Scene()

    g = Transformable(Cube(Material((.3,.8,.2))))
    g.scale(150,1,150)
    scene.add(g)

    h = Transformable(Sphere((0,0,0), 1, Material((.6,.7,.2))))
    h.scale(15,20,15).translate(-40,0, 40)
    scene.add(h)

    b = Transformable(Mesh("barn.off", Material((.8,0,0))))
    b.scale(40,40,40)
    b.rotate(-45,(0,1,0))
    scene.add(b)

    scene.setEyeDistance(80)
    scene.setWindowSize(75,75)
    scene.setView((0, 75, 200), (0,0,0), (0,1,0))
    scene.setLight((60, 200, 200), (1,1,1))
    scene.setAmbient((.5,.5,.5))
    scene.setBackground((1,1,1))

    img = Image(res,res)
    t1 = time.time()
    scene.rayTrace(img, Progress(res).show)
    t2 = time.time()

    name = sys.argv[0].split(".")[0]
    img.savePPM("images/{1}-{0}.ppm".format(res, name))
    print("render time = ", t2-t1)
    
main(int(sys.argv[1]))



