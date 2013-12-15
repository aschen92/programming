#sunset2.py

import sys
from image import *
from ppmmovie import *


def sunset(img, viewer=None):
    w,h = img.size
    for col in range(w):
        for row in range(h):
            loc = (col,row)
            r,g,b = img.getPixel(loc)
            img.setPixel(loc, (r,int(round(g*.7)),int(round((b*.7)))))
        if viewer:
            viewer.show(img)

def main():
    infile = sys.argv[1]
    outfile = sys.argv[2]
    img = Image(0,0)
    img.loadPPM(infile)
    sunset(img, Movie())
    img.savePPM(outfile)

main()
