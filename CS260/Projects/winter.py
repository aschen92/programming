# scene1.py -- VPython Christmas Scene
from visual import *

#set some scene parameters
scene.ambient=.4            # brighten scene with more ambient light
scene.background=(.7,.7,1)  # light blue
#scene.stereo = "passive"   # choices "redblue" or "passive" 
#scene.stereodepth = 1.5    # 0 front edge screen,
                            # 1 center on screen
                            # 2 back edge on screen
                          
#create a grey pentagonal base for the scene
basePts=[(-100,-100,-100),(100,-100,-100),
         (100,-100,100),(0,-100,150),(-100,-100,100)]
convex(pos=basePts,color = (.75,.75,.75))

# put a snowman on the right
# three white spheres
sphere(pos=(40,-70,0),radius=33)
sphere(pos=(40,-16,0),radius=27)
sphere(pos=(40,26,0),radius=22)

# coal eyes and carrot nose and red mouth
cone(pos=(40,26,20),radius=5,axis=(0,0,25),color=color.orange)
sphere(pos=(46,32,19),radius=4,color=color.black)
sphere(pos=(34,32,19),radius=4,color=color.black)
ellipsoid(pos=(40,20,19),length=18,height=8,width=5,color=color.red)

# top hat
cylinder(pos=(40,44,0),radius=20,axis=(0,2,0),color=color.black)
cylinder(pos=(40,44,0),radius=11,axis=(0,20,0),color=color.black)

# halo
ring(pos=(40,75,0), radius=15, axis=(0,1,0), color=color.yellow)

# put a small evergreen on the left
cone(pos=(-50,-70,0),radius=40,axis=(0,150,0),color=color.green)
cylinder(pos=(-50,-100,0),radius=10,axis=(0,30,0),color=(.627,.322,.176))

# with a present under it
box(pos=(-60,-85,55),length=25,width=25,height=25,color=color.red)
box(pos=(-60,-85,55),length=25.3,width=4,height=25.3,color=color.yellow)
box(pos=(-60,-85,55),length=4,width=25.3,height=25.3,color=color.yellow)

helix(pos=(-60,-77 ,55),radius=4,length=12,
      axis=(1,2,1),color=color.yellow, coils=5, thickness=1.5)

helix(pos=(-60,-77 ,55),radius=4, length=10,
      axis=(-1,2,1),color=color.yellow,coils=5,thickness=1.5)

#example of pyramid
pyramid(pos=(-50,-100,-50), size=(40,20,20), axis=(0,1,0),color=color.cyan)
