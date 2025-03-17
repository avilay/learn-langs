"""
When the parent process is killed, the child also dies.
"""

from multiprocessing import Process, Queue
from termcolor import cprint
from colorama import init
import signal
import sys
import os

init()

def f(q):
    cprint('Starting f', 'yellow')
    try:
        while True:
            msg = q.get()
            cprint(msg, 'yellow')
    except KeyboardInterrupt:
        cprint('Exiting f', 'yellow')

def shutdown_handler(signal, frame):
    cprint(f'Got {signal}. Exiting f2', 'green')
    sys.exit(1)

def f2(q):
    cprint(f'pid: {os.getpid()}', 'green')
    signal.signal(signal.SIGINT, shutdown_handler)
    signal.signal(signal.SIGTERM, shutdown_handler)
    # signal.signal(signal.SIGKILL, shutdown_handler)
    while True:
        cprint('Starting f2', 'green')
        msg = q.get()
        cprint(msg, 'green')

def f3(q):
    cprint('Starting f3', 'green')
    while True:
        msg = q.get()
        cprint(msg, 'green')
    cprint('Exiting f3', 'green')


def main1():
    print(f'pid: {os.getpid()}')
    q: Queue = Queue()
    p = Process(target=f2, args=(q,))
    p.start()
    try:
        while True:
            msg = input('Enter message to send:')
            q.put(msg)
    except KeyboardInterrupt:
        print('Exiting main')


def main2():
    print(f'pid: {os.getpid()}')
    q = Queue()
    p = Process(target=f3, args=(q,))
    p.start()
    for i in range(3):
        msg = input(f'{i}. Enter message to send:')
        q.put(msg)
    q.close()
#     p.terminate()
    # p.close()
    cprint('Exiting main')


if __name__ == '__main__':
    # main1()
    main2()
