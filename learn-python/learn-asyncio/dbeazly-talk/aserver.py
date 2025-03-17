from typing import Tuple, Dict, Generator
import socket
from fib import fib
from collections import deque
from select import select

BACKLOG = 5
WhatWhy = Tuple[str, socket.socket]
NoneType = type(None)
Task = Generator[WhatWhy, NoneType, NoneType]

tasks: deque = deque()
recv_wait: Dict[socket.socket, Task] = {}
send_wait: Dict[socket.socket, Task] = {}

def run():
    while any([tasks, recv_wait, send_wait]):
        while not tasks:
            # No active tasks
            # Wait for I/O
            # Running next on any of the recv or send tasks is just going
            # to block on the actual blocking call. No point doing that unless we are
            # sure that there is something ready and the blocking call will complete immediately
            # select uses the OS epoll mechanism s.t it will send an event if there is data
            # available in any of the sockets
            can_recv, can_send, _ = select(recv_wait, send_wait, [])
            for s in can_recv:
                tasks.append(recv_wait.pop(s))
            for s in can_send:
                tasks.append(send_wait.pop(s))

        task = tasks.popleft()
        try:
            why, what = next(task)  # run to the yield
            if why == "recv":
                recv_wait[what] = task
            elif why == "send":
                send_wait[what] = task
            else:
                raise RuntimeError("KABOOM!")
        except StopIteration:
            print("Task done")

def fib_server(address: Tuple[str, int]) -> Task:
    sock: socket.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(address)
    sock.listen(BACKLOG)
    while True:
        yield 'recv', sock
        incoming: Tuple[socket.socket, Tuple[str, int]] = sock.accept()  # blocking
        client, addr = incoming
        print("Connection", addr)
        tasks.append(fib_handler(client))

def fib_handler(client) -> Task:
    while True:
        yield 'recv', client
        req: bytes = client.recv(100)  # blocking
        if not req:
            break
        n = int(req)
        result = fib(n)
        resp = str(result).encode('ascii') + b'\n'
        yield 'send', client
        client.send(resp)  # blocking
    print("Closed")


tasks.append(fib_server(('', 25000)))
run()
