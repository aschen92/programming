# test1.py
# usage: python test1.py 320 240

# A scene with multiple spheres. Using camera on the z axis.

from __future__ import division, print_function

import time
from random import random

from tracer1 import *
from image import *


def test(width, height):

    name = "images/test1a" # root of result file name

    # create a group with a bunch of spheres
    scene = Scene()
    scene.add(Sphere(pos=(0, 300, -1200), radius=200, color=(1,0,0)))
    scene.add(Sphere(pos=(-80, 150, -1200), radius=200, color=(0,1,0)))
    scene.add(Sphere(pos=(70, 100, -1200), radius=200, color=(0,0,1)))
    for x in range(-2,3):
        for z in range(2,8):
            scene.add(Sphere(pos=(x*200, -300, z * -400),
                             radius=40,
                             color = (random(),random(),random())))
            
    scene.setBackground((1,1,1))

    # set eye position
    scene.setEyeDistance(200)

    # set viewing window width and height
    scene.setWindowSize(400, 300)

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

