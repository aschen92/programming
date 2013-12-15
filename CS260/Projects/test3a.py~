# test3a.py
# usage: python test3a.py 320 240

# A scene with multiple spheres. Using camera on the z axis.

from __future__ import division, print_function

import time
from random import random

from tracer3 import *
from image import *


def test(width, height):

    name = "images/test3a" # root of result file name

    # create a group with a bunch of spheres
    scene = Scene()
    scene.add(Sphere(pos=(0, 300, -1200), radius=200, color=GOLD))
    scene.add(Sphere(pos=(-80, 150, -1200), radius=200, color=COPPER))
    scene.add(Sphere(pos=(70, 100, -1200), radius=200, color=BRASS))
    for x in range(-2,3):
        for z in range(2,8):
            scene.add(Sphere(pos=(x*200, -300, z * -400),
                             radius=40,
                             color = Material((random(),random(),random()))))
            
    scene.setBackground((1,1,1))

    # set up the view
    scene.setEyeDistance(200)
    scene.setWindowSize(400, 300)
    scene.setView((20,20, 0), (0, 0,-1200), (-1,0,0))

    # set up lighting
    scene.setLight((-1000,1500, 0), (1,1,1))
    scene.setAmbient((.5,.5,.5))
    

    # create an Image to draw in
    img = Image(width, height)

    # time rendering of the scene
    t1 = time.time()
    #scene.rayTrace(img, Movie(img).show)
    scene.rayTrace(img, Progress(img.size[1]).show)
    #scene.rayTrace(img)
    t2 = time.time()

    # save to an image
    img.savePPM(name + "-{0}x{1}.ppm".format(width,height))

    return t2-t1
    
if __name__ == "__main__":
    import sys
    print(test(int(sys.argv[1]), int(sys.argv[2])))

