"""
Here one coroutine is being run in an event loop like before. But it in turn awaits the completion of two other
coroutines inside it. These sub-coroutines will execute one after the other, and not "in parallel" as one would expect.
In order to get parallel execution, see tasks.py.
"""

import asyncio
import time
import avilabsutils as utils


async def mycoro1():
    utils.print_now('mycoro1: Doing something small')
    time.sleep(3)
    await asyncio.sleep(2)

    utils.print_now('mycoro1: Doing something big')
    time.sleep(5)
    await asyncio.sleep(2)

    utils.print_now('mycoro1: Almost done')
    return 42


async def mycoro2():
    utils.print_now('mycoro2: Doing something big')
    time.sleep(5)
    await asyncio.sleep(2)

    utils.print_now('mycoro2: Almost done')
    return 24


async def start():
    result1 = await mycoro1()
    result2 = await mycoro2()
    utils.print_now('Doing some more work in start')
    time.sleep(2)
    return result1 + result2


def main():
    loop = asyncio.get_event_loop()
    try:
        result = loop.run_until_complete(start())
        print(f'Got back {result}')
    finally:
        loop.close()


if __name__ == '__main__':
    main()
