from visual import *

ball = sphere(pos = (-10,10,10), color= color.orange)
box(width=20, height = .1,length=20)


ball.vel = vector(1,0,-1)


dt = .01
elasticity = .9
scene.mouse.getclick()
trail = curve(radius = .3)
while True:
    rate(100)
    ball.pos = ball.pos + dt*ball.vel
    trail.append(ball.pos)
    if ball.y<1 and ball.vel.y < 0 and abs(ball.x) < 10 and abs(ball.z)<10:
        ball.vel.y = -ball.vel.y*elasticity
    else:
        ball.vel = ball.vel - dt*vector(0,9.8,0)
