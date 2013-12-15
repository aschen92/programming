# ppmmovie.py
# by: John Zelle 4/26/12
#   Display sequence of ppm images using external video viewer.
#   For use with Image objects to provide animated display.
#   This module requires VLC media player and pnmto4ym to be
#   installed on the target system.

from subprocess import *

viewercommand = "pnmtoy4m -n 0 -r 2>/dev/null | vlc - 2>/dev/null"

class Movie(object):
    def __init__(self):
        self.player = Popen(viewercommand, shell=True, stdin=PIPE).stdin

    def show(self, img):
        img.tofile(self.player)
