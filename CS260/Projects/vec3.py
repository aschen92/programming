# vec3.py
# Basic 3D geometry objects
# Aaron Schendel

from __future__ import division, print_function

from math import sqrt

class Vector3(object):
    """A vector in 3-space. Used to represent either a point or a
    displacement.
    """

    def __init__(self, x=0.,y=0.,z=0.):
        """
        >>> v = Vector3(1,2,3)
        >>> v.x, v.y, v.z
        (1.0, 2.0, 3.0)
        """

        self.x = float(x)
        self.y = float(y)
        self.z = float(z)
        

    def __repr__(self):
        """Printable representation of the vector
        
        >>> Vector3(1,2,3)
        Vector3(1.0,2.0,3.0)
        """
        
        return "Vector3({0},{1},{2})".format(self.x, self.y, self.z)


    def __rmul__(self, i):
        """Multiplication by a preceeding scalar

        >>> 3*Vector3(1,2,3)
        Vector3(3.0,6.0,9.0)
        """

        return Vector3(self.x*i,self.y*i,self.z*i)


    def __mul__(self,i):
        """Multiplication by succeeding scalar
        
        >>> Vector3(1,2,3)*3
        Vector3(3.0,6.0,9.0)
        """

        return Vector3(self.x*i,self.y*i,self.z*i)


    def __add__(self, other):
        """Addition of two vectors
        >>> Vector3(1,2,3) + Vector3(-3,1,2.5)
        Vector3(-2.0,3.0,5.5)
        """

        return Vector3(self.x + other.x, self.y + other.y, self.z + other.z)
        
    
    def __sub__(self, other):
        """Subtraction of two vectors
        >>> Vector3(1,2,3) - Vector3(-3,1,2.5)
        Vector3(4.0,1.0,0.5)
        """
        

        return Vector3(self.x - other.x, self.y - other.y, self.z - other.z)
        

    def __neg__(self):
        """negation
        >>> -Vector3(1,-2,3)
        Vector3(-1.0,2.0,-3.0)
        """

        return Vector3(self.x * -1, self.y * -1, self.z * -1)
        
    
    def mag(self):
        """magnitude (length) of vector
        >>> Vector3(1,2,3).mag()
        3.7416573867739413
        """

        return sqrt(self.x**2 + self.y**2 + self.z**2)


    def mag2(self):
        """Square of magnitude
        >>> Vector3(1,2,3).mag2()
        14.0
        """

        return self.mag() ** 2
        

    def normalize(self):
        """Make this vector a unit vector
        >>> v = Vector3(1,2,3)
        >>> v.normalize()
        >>> v
        Vector3(0.267261241912,0.534522483825,0.801783725737)
        """
        
        a = self.mag()
         
        self.x = self.x/a
        self.y = self.y/a
        self.z = self.z/a
        

    def normalized(self):
        """ return normalized version of this vector
        >>> v = Vector3(1,2,3)
        >>> v.normalized()
        Vector3(0.267261241912,0.534522483825,0.801783725737)
        >>> v
        Vector3(1.0,2.0,3.0)
        """

        vector = Vector3(self.x,self.y,self.z)
        vector.normalize()
        
        return vector
        
        
    def dot(self, other):
        """return dot product of self and other
        >>> Vector3(1,2,3).dot(Vector3(4,5,6))
        32.0
        """

        return self.x * other.x+self.y * other.y+ self.z * other.z

        
        

    def cross(self, other):
        """return cross product of self and other
        >>> Vector3(1,2,3).cross(Vector3(4,5,6))
        Vector3(-3.0,6.0,-3.0)
        """
        
        a = self.mag()

        return Vector3(self.y * other.z - other.y * self.z, self.z * other.x - self.x * other.z, self.x * other.y - self.y * other.x)
        

    def normal(self, other):
        """ return a unit vector orthognal to self and other
        
        >>> Vector3(1,2,3).normal(Vector3(4,5,6))
        Vector3(-0.408248290464,0.816496580928,-0.408248290464)
        """

        x = self.cross(other)
        x.normalize()
    
        return x

    def tuple(self):
        """return components of vector as a tuple

        >>> Vector3(1,2,3).tuple()
        (1.0, 2.0, 3.0)
        """

        return (self.x,self.y,self.z)
        

if __name__ == "__main__":
    import doctest
    doctest.testmod()
