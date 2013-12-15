#grayscale.py

from __future__ import division, print_function

import sys
from image import *

def grayscale(img):
    w,h = img.size
    for col in range(w):
        for row in range(h):
            loc = (col,row)
            r,g,b = img.getPixel(loc)
            l = int(round(.299 * r)) + int(round(.589 * g)) + int(round(.114 * b))
            img.setPixel(loc,(l,l,l))

def main():
    infile = sys.argv[1]
    outfile = sys.argv[2]
    img = Image(0,0)
    img.loadPPM(infile)
    grayscale(img)
    img.savePPM(outfile)

main()
