#!/usr/bin/env python

import sys

HOWMANY = 1000

def main():
    for i in range(HOWMANY):
        sys.stdout.write("a")
        sys.stdout.flush()

main()
