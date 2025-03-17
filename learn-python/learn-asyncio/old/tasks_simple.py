import asyncio
import avilabsutils as utils
import time

task = None


async def mycoro1(task_id='default-task'):
    utils.print_now(f'mycoro2 - {task_id}: Doing something big')
    time.sleep(5)
    await asyncio.sleep(2)

    utils.print_now(f'mycoro2 - {task_id}: Done')


async def create_mycoro1(loop):
    global task
    task = loop.create_task(mycoro1())
    utils.print_now('Created task')


async def start(loop):
    await loop.create_task(create_mycoro1(loop))
    utils.print_now('Start completed')
    await task


def main():
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(start(loop))
        utils.print_now('Loop complete.')
    finally:
        loop.close()


if __name__ == '__main__':
    main()
