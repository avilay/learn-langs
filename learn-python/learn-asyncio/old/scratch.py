import asyncio
import avilabsutils as utils
import random

agents = ['strange', 'charmed']
async def get_agent(loop):
    """
    Either get an agent or wait indefinitely until one is available
    """
    while True:
        await asyncio.sleep(5)
        if agents:
            return agents.pop()


async def process(loop, work_q):
    utils.print_now('process: start')
    agent = await get_agent(loop)
    utils.print_now(f'{agent}: start')
    if agent == 'strange':
        sleep_secs = 10
    else:
        sleep_secs = random.randint(2, 5)
    while not work_q.empty():
        try:
            # utils.print_now(f'{agent}: waiting to get item from queue')
            url = work_q.get_nowait()
            utils.print_now(f'{agent} processing {url}')
            await asyncio.sleep(sleep_secs)
            work_q.task_done()
        except asyncio.QueueEmpty:
            pass
    utils.print_now(f'process - {agent}: stop')


async def start(loop):
    work_q = asyncio.Queue()
    for url in ['file1', 'file2', 'file3', 'file4', 'file5']:
        await work_q.put(url)

    processors = [loop.create_task(process(loop, work_q)) for _ in range(work_q.qsize())]
    # await asyncio.wait(processors)
    await work_q.join()
    [processor.cancel() for processor in processors]


def main():
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(start(loop))
    finally:
        loop.close()


if __name__ == '__main__':
    main()
