import asyncio
import functools
import time


# Must accept future as the first param
def callback(future, n):
    print('{}: future done: {}'.format(n, future.result()))


async def mycoro(future):
    """
    This function sets the result of future, but the callbacks are called
    only when it yeilds control back to the event loop. To prove this put
    a time.sleep before exiting the function. The callbacks will not execute
    until mycoro comes out of sleep.
    In contrast if time.sleep is replaced with await sleep, which really
    means yeild the control back to the event loop, the callbacks will
    be executed first before mycoro exits.
    """
    print('In some_func')
    future.set_result('the result')
    await asyncio.sleep(3)
    # time.sleep(3)
    print('Exiting some_func')


def main():
    loop = asyncio.get_event_loop()
    try:
        all_done = asyncio.Future()
        all_done.add_done_callback(functools.partial(callback, n=1))
        all_done.add_done_callback(functools.partial(callback, n=2))
        loop.run_until_complete(mycoro(all_done))
    finally:
        loop.close()


if __name__ == '__main__':
    main()
