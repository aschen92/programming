#sepia.py

from __future__ import division, print_function

import sys
from image import *

def sepia(img):
    w,h = img.size
    for col in range(w):
        for row in range(h):
            loc = (col,row)
            r,g,b = img.getPixel(loc)
            rabbit = min(255,int(round((r * .393)) + int(round(g * .769)) + int(round(b * .189))))
            gary = min(255,int(round((r * .349)) + int(round(g * .686)) + int(round(b * .168))))
            baloon = min(255,int(round((r * .272)) + int(round(g * .534)) + int(round(b * .131))))
            img.setPixel(loc,(rabbit,gary,baloon))

def main():
    infile = sys.argv[1]
    outfile = sys.argv[2]
    img = Image(0,0)
    img.loadPPM(infile)
    sepia(img)
    img.savePPM(outfile)

main()
