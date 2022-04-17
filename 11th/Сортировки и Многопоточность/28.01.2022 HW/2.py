import multiprocessing as mp
import math
import time

def check(p):
    for i in range(2, int(math.sqrt(p))+1):
        if p%i == 0:
            return False
    return p > 1

def generate(lock, pl, update, last):
    while True:
        last = last+1
        if check(last):
            lock.acquire()
            try:
                pl.append(last)
                update = True
            finally:
                lock.release()
                return 

def consume(lock, pl, update):
    if update:
        lock.acquire()
        try:
            for t in pl:
                print("New prime found! -", t)
                update = False
            pl = []
        finally:
            lock.release()

def thg(lock, pl, update, last):
    while True:
        generate(lock, pl, update, last)
        #sprint(update, pl, last)
        #it's permitted to uncomment this: 
        time.sleep(0.1)

def thc(lock, pl, update):
    while True:
        consume(lock, pl, update)
        print(lock, pl, update)

if __name__ == '__main__':
    #mp.set_start_method('spawn')

    lock = mp.Lock()
    pl = []
    update = False
    last = 1

    #parent_conn, child_conn = mp.Pipe()
    th1 = mp.Process(target=thg, args=(lock,pl,update, last))
    th2 = mp.Process(target=thc, args=(lock,pl,update))

    th1.start()
    th2.start()
