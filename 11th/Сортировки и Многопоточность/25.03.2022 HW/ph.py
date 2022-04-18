import threading
import multiprocessing
import time
import sys

forks = [threading.Lock() for i in range(7)]
transaction_counter = 0
transaction_lock = threading.Lock()

def take_fork(n,f,do_take):
    global transaction_counter
    global transaction_lock
    transaction_lock.acquire()

    print("Transaction %d: filosof %d %s left fork %d" % (transaction_counter, n, ("takes" if do_take else "returns"), f))
    sys.stdout.flush()
    transaction_counter += 1
    transaction_lock.release()

    if do_take:
        forks[f].acquire()
    else:
        forks[f].release()

def naiveFilosofLife(num):
    left = num
    right = (num + 1) % 7
    while True:
        take_fork(num,left,True)
        take_fork(num,right,True)
        print("Filosof %d: eating" % num)
        sys.stdout.flush()
        take_fork(num,left,False)
        take_fork(num,right,False)
        print("Filosof %d: thinking about philosopy" % num)
        sys.stdout.flush()

th = [threading.Thread(target = naiveFilosofLife, args=(i,)) for i in range(7)]

for t in th:
    t.start()
#e1.set()
for t in th:
    t.join()
