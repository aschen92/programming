# models3.py
#   modifications to models.py to incorporate meshes

#----------------------------------------------------------------------
def Mesh(fname, color, smooth=False, revnormals=False, recenter=False):
    m = Group()
    data = MeshData(fname, smooth, revnormals)
    if recenter:
        data.recenter()
    for verts, normals in data.triangleIter():
        a,b,c = verts
        m.add(Triangle(a,b,c, color, normals))
    return s

#----------------------------------------------------------------------
class Triangle:
    # Changes here to incorporate meshes are a bit ad-hoc. We now
    #    allow an optional list of 3 normals to be provided. This
    #    is backwards compatible with previous triangle usage.
    
    def __init__(self, v0, v1, v2, color, normals=[]):
        verts = self.vertices = [Vector3(*v0), Vector3(*v1), Vector3(*v2)]
        self.color = color
        if normals != []:  # vertex normals were provided, save them
            self.normals = [Vector3(*n) for n in normals]
        else:              # otherwise compute and save a face normal
            self.normals = []
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
            return False

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

        # TO DO: insert code here to compute normal.
        #   if normals were provided, compute weighted average using
        #      barycentric coordinates to interpolate.
        #   Otherwise: just use the face normal computed in the constructor


        rec.fill(t, ray.pointAt(t), normal, self.color)
        return True


