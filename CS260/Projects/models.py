from __future__ import division, print_function

# models.py
# by: John Zelle

# Models for the simple ray tracer

from vec3 import *
from util import *
from matrix import *
from trans3 import *
from meshdata import *


# Objects for Scene Modeling
#----------------------------------------------------------------------
class Sphere:

    def __init__(self, pos, radius, color):
        self.center = Vector3(*pos)
        self.radius = radius
        self.color = color

    def intersect(self, ray, interval, rec=None):
        e = ray.start
        d = ray.dir
        c = self.center
        ec = e-c
        r = self.radius
        discrim = dotProd(d, ec)**2 - dotProd(d,d)*((dotProd(ec,ec)-r**2))
        if discrim <= 0:
            return False
        discrt = sqrt(discrim)
        num1 = dotProd(-d, ec)
        dd = d.mag2()
        t1 = (num1 - discrt)/dd
        if t1 in interval:
            self.setInfo(ray, t1, rec)
            return True
        t2 = (num1 + discrt)/dd
        if t2 in interval:
            self.setInfo(ray, t2, rec)
            return True

    def setInfo(self, ray, t, rec):
        pt = ray.pointAt(t)
        norm = (pt-self.center)
        norm.normalize()
        rec.fill(t, pt, norm, self.color)

#----------------------------------------------------------------------
class Group:

    def __init__(self, background=(0,0,0)):
        self.objects = []

    def add(self, surface):
        self.objects.append(surface)

    def intersect(self, ray, interval, rec=None):
        hit = False
        for surf in self.objects:
            if surf.intersect(ray, interval, rec):
                hit = True
                interval.max = rec.t
        return hit

#----------------------------------------------------------------------
def Mesh(fname, color, smooth=False, revnormals=False, recenter=False):
    m = Group()
    data = MeshData(fname, smooth, revnormals)
    if recenter:
        data.recenter()
    for verts, normals in data.triangleIter():
        a,b,c = verts
        m.add(Triangle(a,b,c, color, normals))
    return m

#----------------------------------------------------------------------
 
class Triangle:

    def __init__(self, v0, v1, v2, color):
        verts = self.vertices = [Vector3(*v0), Vector3(*v1), Vector3(*v2)]
        self.color = color
        vec02 = verts[2] - verts[0]
        vec01 = verts[1] - verts[0]
        norm = crossProd(vec01, vec02)
        norm.normalize()
        self.faceNormal = norm

    def intersect(self, ray, interval, rec=None):
        a,b,c = self.vertices
        d = ray.dir
        e = ray.start
        a1 = a.x-b.x
        b1 = a.y-b.y
        c1 = a.z-b.z
        d1 = a.x-c.x
        e1 = a.y-c.y
        f1 = a.z-c.z
        g1 = d.x
        h1 = d.y
        i1 = d.z
        ei_hf = e1 * i1 - h1 * f1
        gf_di = g1 * f1 - d1 * i1
        dh_eg = d1 * h1 - e1 * g1
        M = a1*ei_hf + b1*gf_di + c1*dh_eg
        if M == 0: # no intersection with the plane of triangle
            return None

        j1 = a.x-e.x
        k1 = a.y-e.y
        l1 = a.z-e.z
        ak_jb = a1*k1-j1*b1
        jc_al = j1*c1-a1*l1
        bl_kc = b1*l1-k1*c1
        t = -(f1*ak_jb + e1*jc_al + d1*(bl_kc))/M
        if not (t in interval): # plane intersection not in interval
            return False
       
        gamma = (i1*ak_jb + h1*jc_al + g1*bl_kc)/M
        if gamma < 0 or gamma > 1: # plane intersection not in triangle
            return False

        beta = (j1*ei_hf+k1*gf_di+l1*dh_eg)/M
        if beta < 0 or beta > 1-gamma:   # plane intersection not in triangle
            return False

        rec.fill(t, ray.pointAt(t), self.faceNormal, self.color)
        return True
#----------------------------------------------------------------------
def Quad(v0, v1, v2, v3, color):
    s = Group()
    s.add(Triangle(v0, v1, v2, color))
    s.add(Triangle(v0, v2, v3, color))
    return s
        
#----------------------------------------------------------------------
class Material:

    def __init__(self, diffuse,        # diffuse (Lambert) coefficients (req.)
                 ambient=None,         # ambient coefficients
                 specular=(.5,.5,.5),  # specular reflection coefficients
                 exponent=30,          # Shinyness exponent, higher = shinier
                 reflect=None):        # ideal reflection coefficients
        if ambient:           # if ambient is not supplied, use diffuse        
            self.ambient = RGB(*ambient)
        else:
            self.ambient = RGB(*diffuse)
        self.diffuse = RGB(*diffuse)
        self.specular = RGB(*specular)
        self.exponent = exponent
        if reflect:
            self.reflect = RGB(*reflect)

    def __repr__(self):
        return "Material({0}, {1}, {2}, {3}, {4})".format(self.ambient,
                                                          self.diffuse,
                                                          self.specular,
                                                          self.exponent,
                                                          self.reflect)

#------------------------------------------------------------
# example materials from Computer Graphics Using OpenGL, by F.S. Hill

BLACK_PLASTIC = Material(diffuse=(.01,.01,.01), ambient=(0,0,0), 
                         specular=(.5,.5,.5), exponent=32)

BRASS = Material((.780392,.568627,.113725), (.329412,.223529,.027451),
                 (.992157,.941176,.807843), 27.8974)

COPPER = Material((.7038,.27048,.0828), (.19125,.0735,.0225),
                 (.256777, .137622, .086014), 12.8)

GOLD = Material((.75164,.60648,.22648), (.24725,.1995,.0745),
                (.628281,.555802,.366065), 51.2)

SILVER = Material((.50754,)*3, (.19225,)*3, (.508273,)*3, 51.2)

#-------------------------------------------------------------

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
            hitpoint = hinfo.point
            normal = hinfo.normal
            normal = transVec(self.ntrans,normal).normalized()
            
            hinfo.point = hitpoint
            hinfo.normal = normal

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

    def refX(self):
        m = refX()
        self.trans = matMult(m, self.trans)
        self.itrans = matMult(self.itrans, m)
        self.ntrans = matTranspose(self.ntrans)


    def rotate(self, theta, v):
        x,y,z = v

        m = rotate(theta, x, y, z)
        self.trans = matMult(m, self.trans)
        mi = rotate(-theta, x, y, z)
        self.itrans = matMult(self.itrans, mi)
        self.ntrans = matTranspose(self.itrans)

        
        return self
        
        
class Cube:

    def __init__(self, color):
        self.color = color




    def intersect(self, ray, interval, hinfo):
       
        xmin = ymin = zmin = -.5
        xmax = ymax = zmax = .5
        
        dir = ray.dir
        xd = dir.x
        yd = dir.y
        zd = dir.z

        start = ray.start
        xe = start.x
        ye = start.y
        ze = start.z        

        
        
        if xd >= 0:
            txmin = (xmin - xe)/xd
            txmax = (xmax - xe)/xd
        else:
            txmin = (xmax - xe)/xd
            txmax = (xmin - xe)/xd
        if yd >= 0:
            tymin = (ymin - ye)/yd
            tymax = (ymax - ye)/yd
        else:
            tymin = (ymax - ye)/yd
            tymax = (ymin - ye)/yd
        if zd >= 0:
            tzmin = (zmin - ze)/zd
            tzmax = (zmax - ze)/zd
        else:
            tzmin = (zmax - ze)/zd
            tzmax = (zmin - ze)/zd
        
        if (txmin > tymax) or (tymin > txmax) or (txmin > tzmax) or (tzmin > txmax) or (tymin > tzmax) or (tzmin > tymax):
            return False

        hit = False
        if txmin in interval:
            pt = start + txmin * dir
            if inbound(pt.z) and inbound(pt.y):
                hit = True
                hinfo.fill(txmin,pt,Vector3(-1,0,0),self.color)
                interval.max = txmin
           
               
        if txmax in interval:
            pt = start + txmax * dir
            if inbound(pt.z) and inbound(pt.y):
                hit = True
                hinfo.fill(txmax,pt,Vector3(1,0,0),self.color)
                interval.max = txmax
               
        if tymin in interval:
            pt = start + tymin * dir
            if inbound(pt.x) and inbound(pt.z):
                hit = True
                hinfo.fill(tymin,pt,Vector3(0,-1,0),self.color)
                interval.max = tymin
               
        if tymax in interval:
            pt = start + tymax * dir
            if inbound(pt.x) and inbound(pt.z):
                hit = True
                hinfo.fill(tymax,pt,Vector3(0,1,0),self.color)
                interval.max = tymax
               
        if tzmin in interval:
            pt = start = tzmin * dir
            if inbound(pt.x) and inbound(pt.y):
                hit = True
                hinfo.fill(tzmin,pt,Vector3(0,0,-1),self.color)
                interval.max = tzmin
               
        if tzmax in interval:
            pt = start = tzmax * dir
            if inbound(pt.x) and inbound(pt.y):
                hit = True
                hinfo.fill(tzmax,pt,Vector3(0,0,1),self.color)
                interval.max = tzmax

        
        return hit


def sign(x):
    if x < 0:
        return -1
    else:
        return 1

def inbound(pt):
    return pt > -.5 and pt < .5
