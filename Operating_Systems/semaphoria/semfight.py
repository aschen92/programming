#semfight.py
# Aaron, Cody, Brett


import semaphores as sem
import time
import random
import _thread as thread
import sys

mutex = sem.Semaphore(1)

def threadrun(tnum, *args):
  
    for i in range(3):
        print('Thread {0} is trying to acquire mutex'.format(tnum))
        mutex.wait()
        print('Thread {0} has acquired mutex'.format(tnum))
        rando = random.randrange(5)
        print('Thread {0} is holding lock for {1} seconds'.format(tnum, rando))
        time.sleep(rando)
        print('Thread {0} is releasing mutex lock'.format(tnum))
        mutex.signal()

    

def lockfight(threads=4):
    print("Running with {0} threads".format(threads))

    for i in range(threads):
        thread.start_new_thread(threadrun, (i,))
    

    time.sleep(threads*15)
if __name__ == "__main__":
    threads = int(sys.argv[1])
    lockfight(threads)

