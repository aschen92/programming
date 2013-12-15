#!/usr/bin/env python

import sys, os

HOWMANY = 1000

def main():
    if os.fork() == 0:
        for i in range(HOWMANY):
            sys.stdout.write("B")
            sys.stdout.flush()

    if os.fork() == 0:
        for i in range(HOWMANY):
            sys.stdout.write("a")
            sys.stdout.flush()

 
main()
