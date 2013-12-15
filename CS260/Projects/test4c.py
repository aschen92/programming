# test4c.py

#   A nice scene from spheres and cubes

from __future__ import division, print_function

from tracer import *

def main(res):
    s = Scene()
    board = Transformable(Cube(Material((.3,.3,.8))))
    thick = .2
    board.scale(3,thick,3).translate(0, -thick/2, 0)
    s.add(board)

    #line = Transformable(Cube(BLACK_PLASTIC))
    line = Transformable(Quad((-.5,.0001,-.5),(.5,.0001,-.5),(.5,.0001,.5),(-.5,.0001,.5),
                              BLACK_PLASTIC))
    line.scale(2.75,.01,.1)
    line1 = Transformable(line).translate(0,0,.5)
    s.add(line1)
    line2 = Transformable(line).translate(0,0,-.5)
    s.add(line2)
    line3 = Transformable(line).rotate(90, (0,1,0)).translate(-.5,0,0)
    s.add(line3)
    line4 = Transformable(line).rotate(90, (0,1,0)).translate(.5,0,0)
    s.add(line4)
    

    oh = Transformable(Sphere((0,0,0), .5, GOLD))
    oh.scale(.70, .3, .70)
    s.add(oh)

    oh2 = Transformable(oh).translate(1, 0, 0)
    s.add(oh2)

    oh3 = Transformable(oh).translate(-1, 0, -1)
    s.add(oh3)

    oh4 = Transformable(oh).translate(0,0,1)
    s.add(oh4)

    x = .2
    exleg1 = Transformable(Sphere((0,0,0), .5, COPPER)).scale(.8, x,x).rotate(45,(0,1,0))
    exleg2 = Transformable(Sphere((0,0,0), .5, COPPER)).scale(.8, x,x).rotate(-45,(0,1,0))
    ex = Group()
    ex.add(exleg1)
    ex.add(exleg2)

    ex1 = Transformable(ex).translate(-1, 0, 0)
    s.add(ex1)

    ex2 = Transformable(ex).translate(-1,0,1)
    s.add(ex2)

    ex3 = Transformable(ex).translate(0,0,-1)
    s.add(ex3)

    ex4 = Transformable(ex).translate(1,0,-1)
    s.add(ex4)
    
    ex5 = Transformable(ex).translate(1,0,1)
    s.add(ex5)
    
    
    s.setView((8, 10, 20), (0,0,0), (0,1,0))
    #s.setView((0, 20, 0), (0,0,0), (0,0,-1))
    #s.setView((0, 0, 20), (0,0,0), (0,1,0))
    
    s.setWindowSize(2,1.5)
    s.setEyeDistance(10)
    s.setLight((0,20, 0), (1,1,1))

    s.setAmbient((.4,.4,.4))
    s.setBackground((1,1,1))

    img = Image(res,int(.75*res+.5))
    t1 = time.time()
    s.rayTrace(img, Progress(.75*res).show)
    t2 = time.time()

    img.savePPM("images/test4c-{0}.ppm".format(res))
    print("render time = ", t2-t1)
    
main(int(sys.argv[1]))

