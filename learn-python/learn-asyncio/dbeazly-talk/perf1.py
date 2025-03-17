"""
GIL Demo: Python programs are pinned to a single core even though multiple cores might be available.

If I run this program I will get around 0.18 secs per request. However, if I then run another instance of the program, the performance will halve because the server is pinned to a single core and can only service one thread at a time.
"""

import socket
import time

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("localhost", 25000))
while True:
    start = time.time()
    sock.send(b'30')
    resp = sock.recv(100)
    end = time.time()
    print(end - start)
