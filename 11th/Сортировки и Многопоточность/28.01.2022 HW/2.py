import multiprocessing as mp
import math
import time

last = 1

def check(p):
    for i in range(2, int(math.sqrt(p))+1):
        if p%i == 0:
            return False
    return p > 1

def generate(conn):
    global last
    while True:
        last = last+1
        if check(last):
            conn.send(last)

def consume(conn):
    print("New prime found! -", conn.recv())

def thg(conn):
    while True:
        generate(conn)
        #sprint(update, pl, last)
        #it's permitted to uncomment this: 
        time.sleep(0.1)

def thc(conn):
    while True:
        consume(conn)

if __name__ == '__main__':
    #mp.set_start_method('spawn')

    parent_conn, child_conn = mp.Pipe()
    th1 = mp.Process(target=thg, args=(child_conn, ))
    th2 = mp.Process(target=thc, args=(parent_conn, ))

    th1.start()
    th2.start()
