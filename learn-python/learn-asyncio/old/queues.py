import asyncio
import time
import random
import avilabsutils as utils


async def consumer(coroid, q):
    print(f'Inside Consumer {coroid}')
    try:
        while True:
            item = await q.get()
            sleep_secs = random.randint(2, 5)
            utils.print_now(f'Consumer {coroid}: Processing {item} for {sleep_secs} seconds')
            time.sleep(sleep_secs)  # Do some work
            await asyncio.sleep(sleep_secs)  # Yeild the thread while waiting for something else to complete
            q.task_done()
    except asyncio.CancelledError:
        utils.print_now(f'Consumer {coroid} ending')


async def kill_consumers(consumers, kill_event):
    await kill_event.wait()
    utils.print_now('Killer: Got kill signal')
    [t.cancel() for t in consumers]


async def producer(queue, event):
    utils.print_now('Producer: Adding first batch of queue items')
    for name in ['anu', 'chiku', 'anika']:
        await queue.put(name)
    await queue.join()
    utils.print_now('Producer: Adding second batch of queue items')
    await asyncio.sleep(3)
    for name in ['lalji', 'aptg', 'avilay']:
        await queue.put(name)
    await queue.join()
    event.set()
    utils.print_now('Producer: All tasks complete')


async def start(loop):
    num_workers = 3
    queue = asyncio.Queue()
    event = asyncio.Event()
    prod = loop.create_task(producer(queue, event))
    consumers = [loop.create_task(consumer(i, queue)) for i in range(num_workers)]
    kc = loop.create_task(kill_consumers(consumers, event))
    await asyncio.wait(consumers + [prod, kc])


def main():
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(start(loop))
    finally:
        loop.close()


if __name__ == '__main__':
    main()
