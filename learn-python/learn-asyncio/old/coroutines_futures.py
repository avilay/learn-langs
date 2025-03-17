"""
Just like a coroutine, which can be run in an event loop and be used
with await, futures can also do both.
"""
import asyncio
import time


def some_func(future):
    print('in some_func')
    time.sleep(3)
    future.set_result(42)


async def start(loop):
    all_done = asyncio.Future()
    loop.call_soon(some_func, all_done)
    result = await all_done
    print(f'Got back {result}')


def main():
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(start(loop))
    finally:
        loop.close()


if __name__ == '__main__':
    main()
