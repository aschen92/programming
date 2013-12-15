# models.py

from vec3 import *
from util import *

# Surfaces for Scene Modeling
#----------------------------------------------------------------------
class Sphere:

    def __init__(self, pos=(0,0,0), radius=1, color=(1,.25,.25)):
        pass

    def intersect(self, ray, interval, rec=None):
        pass

#----------------------------------------------------------------------
class Group:

    def __init__(self):
        pass
    
    def add(self, surface):
        pass
    
    def intersect(self, ray, interval, rec=None):
        pass
    
        
