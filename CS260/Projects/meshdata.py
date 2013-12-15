# meshdata.py

from __future__ import division, print_function
from vec3 import *

class MeshData:
    """MeshData objects can load 3VN and OFF formatted mesh files
          The resulting object has public attributes containing data
          about the mesh:
              verts is a list of triples for vertices of the mesh
              norms is a list of triples for normals of the mesh
              faces is a list of face pairs
                  a face pair contains two lists: (vertis, normis)
                  vertis is the list of vertex indices
                  normis is the corresponding list of normal indicies
    """

    def __init__(self, fname, smooth=False, revnormals=False):
        self.vertices = [] # list of triples
        self.normals = []   # list of triples
        self.faces = []     # face is a pair (verts, norms)
        self.loadOFF(fname, smooth)
        if revnormals:
            for i in range(len(self.normals)):
                x,y,z = self.normals[i]
                self.normals[i] = (-x,-y,-z)


    def faceIter(self):
        # produce faces as ([vertices], [normals])
        for vertis, normis in self.faces:
            verts = [self.vertices[i] for i in vertis]
            norms = [self.normals[i] for i in normis]
            yield(verts,norms)


    def triangleIter(self):
        # produce triangles as ([vertices], [normals])
        for vs, ns in self.faceIter():
            root = vs[0]
            for i in range(1,len(vs)-1):
                yield ([vs[i],vs[i+1],root],[ns[i],ns[i+1],ns[0]])

    #    for i in range(1,len(vertices-1)):
    #        yield(vertices[i], vertices[i+1], vertices[0])
            
        
    def loadOFF(self, fname, smooth=False):
        # load from an OFF file. Normals are generated automatically
        
        infile = open(fname,"rb")

        x = infile.readline()
        
        numVertices, numFaces, nill = infile.readline().split()
        
        numVertices, numFaces = int(numVertices), int(numFaces)

        for i in range(numVertices):
            x,y,z = infile.readline().split()
            x,y,z = float(x),float(y),float(z)
            self.vertices.append((x,y,z))
            
        for i in range(numFaces):
            line = infile.readline().split()
            numVert = int(line[0])
            face = []
            for i in range(1,numVert+1):
                face.append(float(line[i]))
            
            
            self.faces.append(tuple(face))
            
                


    def recenter(self):
        # translate all vertices to move centroid to origin
        pass


    def _face_normal(self, face):
        # helper to compute the normal to a face
        #   face is a list of vertex indexes.
        pass

    
    def _average_normal(self, faceList):
        # helper to compute the "average" normal over a set of faces
        #   facelist is a list of faces, where a face is a list of vertex
        #   indexes
        #use in
        pass

