from __future__ import division, print_function

import sys

from models import *
from tracer import *
from image import *

def main(res):
    scene = Scene()

    # tranformable unit sphere
    cube1 = Transformable(Cube(Material((.8,.3,.3))))
    cube1.scale(20,5,5)
    cube1.rotate(45, (1,1,1))
    cube1.translate(0,10,-50)
    # alternatively, these could all be done at once:
    #cube1.scale(20, 5, 5).rotate(45, (1,1,1)).translate(0,10,-50)
    scene.add(cube1)

    scene.setEyeDistance(200)
    scene.setWindowSize(50,50)
    scene.setView((0, 20, 200), (0,0,-50), (0,1,0))
    scene.setLight((0, 100, 100), (1,1,1))
    scene.setAmbient((.4,.4,.4))
    scene.setBackground((1,1,1))

    img = Image(res,res)
    t1 = time.time()
    scene.rayTrace(img, Progress(res).show)
    t2 = time.time()

    img.savePPM("images/test4b-{0}.ppm".format(res))
    print("render time = ", t2-t1)
    
main(int(sys.argv[1]))
