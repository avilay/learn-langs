from collections import deque
from typing import Callable, Generator, Any, List


class EventLoop:
    def __init__(self):
        self._ready_q = deque()

    def run(self):
        while True:
            try:
                ntodo = len(self._ready_q)
                for _ in range(ntodo):
                    func = self._ready_q.popleft()
                    func()
            except KeyboardInterrupt:
                break

    def call_soon(self, func: Callable):
        self._ready_q.append(func)


class Future:
    def __init__(self, loop: EventLoop):
        self._loop = loop
        self._callabcks: List[Callable] = []
        self._result: Any = None

    def add_callback(self, callback: Callable):
        self._callabcks.append(callback)

    def set(self, result: Any):
        self._result = result
        for callback in self._callabcks:
            self._loop.call_soon(callback)


def set_callback(future: Future, result: Any):
    def set_future():
        future.set(result)

    return set_future


class Task:
    def __init__(self, coro: Generator, loop: EventLoop):
        self._coro = coro
        self._loop = loop
        self._loop.call_soon(self.step)

    def step(self):
        try:
            future = self._coro.send(None)
            if future is not None:
                future.add_callback(self.step)
        except StopIteration:
            pass


def cfunc_2(loop: EventLoop):
    print("cfunc_2: Starting")
    future = Future(loop)
    loop.call_soon(set_callback(future, None))
    yield future
    print("cfunc_2: Stopping")


def cfunc_1(loop: EventLoop):
    print("cfunc_1: Starting")
    yield from cfunc_2(loop)
    print("cfunc_1: Stopping")


def busy():
    print("busy: Starting")
    s = 0
    for i in range(10_000):
        for j in range(10_000):
            s += i + j
    print("busy: Ending")
    return s


loop = EventLoop()
task = Task(cfunc_1(loop), loop)
loop.call_soon(busy)
loop.run()
