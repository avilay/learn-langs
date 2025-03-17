from typing import Tuple
import socket
from threading import Thread
from fib import fib

BACKLOG = 5

def fib_server(address: Tuple[str, int]):
    sock: socket.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(address)
    sock.listen(BACKLOG)
    while True:
        incoming: Tuple[socket.socket, Tuple[str, int]] = sock.accept()
        client, addr = incoming
        print("Connection", addr)
        Thread(target=fib_handler, args=(client,)).start()

def fib_handler(client):
    while True:
        req: bytes = client.recv(100)
        if not req:
            break
        n = int(req)
        result = fib(n)
        resp = str(result).encode('ascii') + b'\n'
        client.send(resp)
    print("Closed")

fib_server(('', 25000))

