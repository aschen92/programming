from visual import *

#set some scene parameters
scene.ambient=.4            # brighten scene with more ambient light
scene.background=(.7,.7,1)  # light blue
                          

basePts=[(-200,-100,-100),(200,-100,-100),
         (200,-100,100),(-200,-100,100)]
convex(pos=basePts,color = (0,1,0), radius = .2)


curve(pos = [(-150,-100,-10),(-150,-50,0),(-150,-100,10)],radius = 3)
curve(pos = [(-150,-50,0),(-150,-25,0),(-125,-42,20)] , radius = 3)
curve(pos = [(-150,-50,0),(-150,-25,0),(-150,-42,-20)] , radius = 3)
curve(pos = [(-150,-50,0),(-150,-25,0),(-150,0,0)] , radius = 3)
sphere(pos = (-150,0,0),radius = 15)


curve(pos = [(150,-100,-10),(150,-50,0),(150,-100,10)],radius = 3)
curve(pos = [(150,-50,0),(150,-25,0),(150,-42,20)] , radius = 3)
curve(pos = [(150,-50,0),(150,-25,0),(125,-42,-20)] , radius = 3)
curve(pos = [(150,-50,0),(150,-25,0),(150,0,0)] , radius = 3)
sphere(pos = (150,0,0),radius = 15)



frisbeeRing = ring(pos = (-110,-42,25), radius = 15, axis = (0,1,0))
frisbeeCyl = cylinder(pos = (-110,-42,25), radius = 14.9, axis = (0,1.45,0))

scene.mouse.getclick()

fact = .1


while True:
    for i in range(3000):
        rate(3000)
        frisbeeRing.pos.x += 1 * fact
        frisbeeRing.pos.z -= .02272727
        frisbeeCyl.pos.x += 1 * fact
        frisbeeCyl.pos.z -= .02272727
        if frisbeeRing.pos.x >= 110:
            break

    for i in range(3000):
        rate(3000)
        frisbeeRing.pos.x -= 1 * fact
        frisbeeRing.pos.z += .02272727
        frisbeeCyl.pos.x -= 1 * fact
        frisbeeCyl.pos.z += .02272727
        if frisbeeRing.pos.x <= -110:
            break

        
        
