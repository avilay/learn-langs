import time
import asyncio
import random
import avilabsutils as utils


async def process(name, queue):
    print(f'process - {name}: starting')
    while not queue.empty():
        item = await queue.get()
        print(f'process - {name}: processing {item}')
        if name == 'neverend':
            while True:
                time.sleep(3)
                await asyncio.sleep(5)
        else:
            time.sleep(2)
            await asyncio.sleep(3)
            time.sleep(2)
            queue.task_done()


async def start(loop):
    queue = asyncio.Queue(2)
    await queue.put('item 1')
    await queue.put('item 2')
    quick = loop.create_task(process('quick', queue))
    neverend = loop.create_task(process('neverend', queue))
    await quick
    print(queue.empty(), queue.qsize())
    await neverend


def main():
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(start(loop))
    finally:
        loop.close()


if __name__ == '__main__':
    main()