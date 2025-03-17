"""
Apart from coroutines, objects of type Future can also be run in an
event loop. Here there is a future-aware function - some_func.
After it does it work, it sets the result in the future object which can
then be retreived from the loop run.
"""


import asyncio
import time


def some_func(future):
    print('in some_func')
    time.sleep(3)
    future.set_result(42)


def main():
    loop = asyncio.get_event_loop()

    try:
        all_done = asyncio.Future()
        loop.call_soon(some_func, all_done)
        result = loop.run_until_complete(all_done)
        print(f'Got back {result}')
    finally:
        loop.close()


if __name__ == '__main__':
    main()
