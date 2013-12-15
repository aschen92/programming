#!/usr/local/bin/python
# Dining Philosophers shell
# Code shamefully borrowed from Dr. John Zelle. Thanks, Dr. Zelle!

# Brett Aaron Cody

# system modules
from random import randint, randrange
import _thread, time, signal
from time import sleep

# local modules
from DPGui import DPDisplay
from semaphores import Semaphore


NUM_PHILOSOPHERS = 5   # changeable from command-line
THINKMAX = 6
EATMAX = 2

def philosopher(i, display, forks):
    # behavior of the ith philosopher
    disp = display
    d = disp

    left = i
    right = (i+1) % NUM_PHILOSOPHERS

    # SOLVING DEAD-LOCK PROBLEM
    if (i % 2 == 0):
        t = left
        left = right
        right = t

    while True:
        d.setPhil(i, 'thinking')
        sleep(randrange(THINKMAX))
        d.setPhil(i, 'hungry')

        forks[left].wait() #grab left fork
        d.setFork(i, 'inuse')
        sleep(1)

        forks[right].wait() #grab right fork
        d.setFork(right, 'inuse')

        d.setPhil(i, 'eating')
        sleep(randrange(EATMAX))

        forks[left].signal() #release left fork
        d.setFork(left, 'free')
        forks[right].signal() #release right fork
        d.setFork(right, 'free')
        
def main():
    d = DPDisplay(NUM_PHILOSOPHERS)
    forks = []
    for i in range(NUM_PHILOSOPHERS):
        forks.append(Semaphore(1))
        d.setFork(i,"free")
    for i in range(NUM_PHILOSOPHERS):
        _thread.start_new_thread(philosopher, (i,d,forks))

    d.pause()  # wait for mouse click to keep main thread alive


# Use command-line argument to change number of philosophers
from sys import argv
if len(argv) > 1:
    NUM_PHILOSOPHERS = int(argv[1])

main()         # Run main program to launch philosophers
