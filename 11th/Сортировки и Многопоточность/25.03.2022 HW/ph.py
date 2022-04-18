import threading
import queue
import time
import sys
import os

forks = [threading.Lock() for i in range(7)]

transaction_counter = 0
transaction_lock = threading.Lock()
state = [-1,-1,-1,-1,-1,-1,-1]

qs = queue.Queue()

def drawtable(tb):
    l = 4*6
    print((" "+"_"*(l//2-1) if tb[0] == 6 else " "*(l//2))+"7"+("_"*(l//2-1)+" " if tb[6] == 6 else " "*(l//2)))
    print(("/" if tb[0] == 6 else " ")+" "*(l-1)+("\\" if tb[6] == 6 else " "))
    for i in range(7):
        print("#   ", end = '')
    print("")

    for i in range(6):
        print((" \\ " if tb[i] == i else "   ")+("/" if tb[i+1] == i else " "), end = '')
    print("")

    for i in range(6):
        print("  "+str(i+1)+" ", end = '')
    print("")

def take_fork(n,f,do_take):
    global transaction_counter
    global transaction_lock
    global state
    global qs

    if do_take:
        forks[f].acquire()
    else:
        forks[f].release()
    
    with transaction_lock:
        state[f] = n if do_take else -1
        
        os.system("cls")
        print("Transaction %d:" % transaction_counter)
        print(state)
        drawtable(state)
        print("")

        transaction_counter += 1

def naiveFilosofLife(num):
    left = num
    right = (num + 1) % 7
    while True:
        take_fork(num,left,True)
        take_fork(num,right,True)
        #print("Filosof %d: eating" % num)
        take_fork(num,left,False)
        take_fork(num,right,False)
        #print("Filosof %d: thinking about philosopy" % num)


def observer(): #optional solution
    global qs

    while True:
        table = qs.get()

        os.system('cls')
        print("Transaction %d:" % table[0])
        print(table[1])
        drawtable(table[1])
        print("")
        sys.stdout.flush()

        qs.task_done()

th = [threading.Thread(target = naiveFilosofLife, args=(i,)) for i in range(7)]

for t in th:
    t.start()

#threading.Thread(target = observer).start()

