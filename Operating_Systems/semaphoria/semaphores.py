#!/usr/bin/env python
# Brett Aaron Cody
#
# In class code/notes from 11/22/13 on the
# semaphore assignment


import _thread # import for python2 use _thread for python3
class Semaphore:
    def __init__(self, value=0):
        self.count = value
        self.queue = []
        self.mutex = _thread.allocate_lock() # thread location

    def wait(self):
        self.mutex.acquire()
        self.count -= 1
        if self.count < 0:
            # out of resources so we are making this process wait
            wlock = _thread.allocate_lock()
            wlock.acquire()
            self.queue.append(wlock)
            self.mutex.release()
            wlock.acquire() # need to acquire twice so that we block. Explanation in the thread documentation
        else:
            self.mutex.release()

    def signal(self):
        self.mutex.acquire() 
        self.count += 1
        if self.count <= 0:
            # need to wake up a process
            wlock = self.queue.pop()

            wlock.release()
        self.mutex.release()

