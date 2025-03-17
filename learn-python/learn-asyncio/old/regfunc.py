import asyncio
import functools
import time


def regular_func(arg, *, kwarg='default'):
    print(f'regular_func invoked with arg={arg} and kwarg={kwarg}')
    time.sleep(3)


async def start():
    print('Exiting start')


def main():
    loop = asyncio.get_event_loop()
    try:
        loop.call_later(0.2, regular_func, 1)
        wrapped = functools.partial(regular_func, kwarg='not default')
        loop.call_soon(wrapped, 2)
        loop.run_until_complete(start())
    finally:
        loop.close()


if __name__ == '__main__':
    main()
