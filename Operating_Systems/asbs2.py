#!/usr/bin/env python

#script asbs2.out
#python3.2 asbs2.py
#ctrl-d
#less asbs2.out

import sys, os

HOWMANY = 1000

def main():
    if os.fork() == 0:
        for i in range(HOWMANY):
            sys.stdout.write("B")
            sys.stdout.flush()

    else:
        pid, status = os.wait()
        if os.fork() == 0:
            for i in range(HOWMANY):
                sys.stdout.write("a")
                sys.stdout.flush()
        else:
            pid, status = os.wait()
    
            print("All Done!")

 
main()
