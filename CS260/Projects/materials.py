from util import *
 
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
