"""
GIL Demo: GIL prioritizes CPU intensive threads over other threads.

This program requests a trivial fibonacci number which does not use much CPU. The number of requests per second will be around 38K. However, if I start another netcat client and request the fibonacci for 40, then the rps drops off a cliff to 115. That is because the GIL is prioritizing the thread computing fib(40) over the thread that is handling this client which is computing fib(1).
"""

import socket
import time
from threading import Thread

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("localhost", 25000))
reqs_per_sec = 0

def monitor():
    global reqs_per_sec
    while True:
        time.sleep(1)
        print(reqs_per_sec, "reqs/sec")
        reqs_per_sec = 0

Thread(target=monitor).start()

while True:
    sock.send(b'1')
    resp = sock.recv(100)
    reqs_per_sec += 1
