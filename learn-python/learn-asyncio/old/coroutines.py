"""
Notice how the coroutine object is run in an event loop.
"""

import asyncio
import time


async def mycoro():
    print('in coro')
    time.sleep(3)
    return 10


def main():
    loop = asyncio.get_event_loop()
    try:
        coro = mycoro()
        result = loop.run_until_complete(coro)  # can only run coroutine objects
        print(f'Got back {result}')
    finally:
        loop.close()


if __name__ == '__main__':
    main()
