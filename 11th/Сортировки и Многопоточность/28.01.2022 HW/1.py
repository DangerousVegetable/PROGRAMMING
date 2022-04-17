import threading
import math
import time

pl = []
update = False
last = 1
lock = threading.Lock()

def check(p):
    for i in range(2, int(math.sqrt(p))+1):
        if p%i == 0:
            return False
    return p > 1

def generate():
    global pl
    global update
    global last
    global lock

    while True:
        last = last+1
        if check(last):
            with lock:
                pl.append(last)
                update = True
            return 

def consume():
    global pl
    global update
    global lock

    if update:
        with lock:
            for t in pl:
                print("New prime found! -", t)
            update = False
            pl = []

def thg():
    while True:
        generate()

        #it's permitted to uncomment this: 
        #time.sleep(0.1)

def thc():
    while True:
        consume()

th1 = threading.Thread(target=thg, args=())
th2 = threading.Thread(target=thc, args=())

th1.start()
th2.start()
