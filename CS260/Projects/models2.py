# models2

#   Transformable and Cube classes to add to ray tracer

from trans3 import *

class Transformable:

    def __init__(self, primitive):
        self.object = primitive
        self.trans = matID(4)    # transform from primitive to world
        self.itrans = matID(4)   # transform from world to primitive
        self.ntrans = matID(4)   # transform normals from primitive to world

    def intersect(self, ray, interval, hinfo):
        # algorithm
        # inverse transform the ray into the primitive's space
        # intersect with the primitive
        # If there is a hit:
        #    Transform the hitpoint and normal back into world space
        # return whether there was a hit
        
        ray2Start = transPt(self.itrans ,ray.start) 
        ray2Dir = transVec(self.itrans ,ray.dir)

        ray2 = Ray(ray2Start, ray2Dir)

        
        if self.object.intersect(ray2,interval,hinfo):

            normal = (hitpoint-self.object.pos).normalized()
            hitpoint = ray2.pointAt(hinfo.pt)
            hinfo.fill( , hitpoint, normal, )

            return True


        return False

    def translate(self, dx, dy, dz):
        m = translate(dx, dy, dz)
        self.trans = matMult(m, self.trans)
        mi = translate(-dx, -dy, -dz)
        self.itrans = matMult(self.itrans, mi)
        self.ntrans = matTranspose(self.itrans)

        return self
    
    def scale(self, sx, sy, sz):
        m = scale(sx, sy, sz)
        self.trans = matMult(m, self.trans)
        mi = scale(1/sx,1/sy,1/sz)
        self.itrans = matMult(self.itrans, mi)
        self.ntrans = matTranspose(self.itrans)


        return self


    def rotate(self, theta, v)
        x,y,z = v.x, v.y, v.z

        m = rotate(theta, x, y, z)
        self.trans = matMult(m, self.trans)
        mi = scale(-theta, x, y, z)
        self.itrans = matMult(self.itrans, mi)
        self.ntrans = matTranspose(self.itrans)

        
        return self
        
        
class Cube:

    def __init__(self, color):
        self.color = color




    def intersect(self, ray, interval, hinfo):
        return False

