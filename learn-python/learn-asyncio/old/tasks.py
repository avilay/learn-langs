import asyncio
import time
import avilabsutils as utils


async def mycoro1(task_id='default_task_id'):
    some_state = ['water']
    utils.print_now(f'mycoro1 - {task_id}: current state: {some_state}. Doing something small')
    time.sleep(3)
    await asyncio.sleep(2)

    some_state.append('air')
    utils.print_now(f'mycoro1 - {task_id}: current state: {some_state}. Doing something big')
    time.sleep(5)
    await asyncio.sleep(2)

    some_state.append('earth')
    utils.print_now(f'mycoro1 - {task_id}: current state: {some_state}. Almost done')
    return 42


async def mycoro2(task_id='default_task_id'):
    utils.print_now(f'mycoro2 - {task_id}: Doing something big')
    time.sleep(5)
    await asyncio.sleep(2)

    utils.print_now(f'mycoro2 - {task_id}: Almost done')
    return 24


async def mycoro3(loop, task_id='default_task_id'):
    utils.print_now(f'mycoro3 - {task_id}: Starting by sleeping')
    asyncio.sleep(5)
    task = loop.create_task(mycoro1('dynamically-added'))
    return await task


async def start_dyn(loop):
    task1 = loop.create_task(mycoro1('initial-task'))
    task2 = loop.create_task(mycoro2('initial-task'))
    task3 = loop.create_task(mycoro3(loop))
    await asyncio.wait([task1, task2, task3])


async def start_tasks_explicitly(loop):
    task1 = loop.create_task(mycoro1())
    task2 = loop.create_task(mycoro2())
    utils.print_now('Tasks created. Sleeping before awaiting')
    time.sleep(2)
    utils.print_now('Awake')
    result1 = await task1
    result2 = await task2

    # This statement is executed only when both the coros are completed
    utils.print_now('Doing some more work in start')
    time.sleep(2)
    return result1 + result2


async def start_tasks_implicitly():
    # wait wraps the coroutines in tasks and executes them just like
    # start_tasks_explicitly
    completed, _ = await asyncio.wait([mycoro1(), mycoro2()])

    # This statement will execute only when both the coros have completed.
    utils.print_now('Doing some more work in start')
    time.sleep(2)
    return [t.result() for t in completed]


async def start_tasks_implicitly_2():
    # Instead of getting results from each completed task individually
    # this is a convenience way of doing it.
    results = await asyncio.gather(mycoro1(), mycoro2())

    # This statement is executed only when both the coros have completed
    utils.print_now('Doing some more work in start')
    time.sleep(2)
    return results


def main():
    loop = asyncio.get_event_loop()
    try:
        # result = loop.run_until_complete(start_tasks_explicitly(loop))
        # result = loop.run_until_complete(start_tasks_implicitly())
        # result = loop.run_until_complete(start_tasks_implicitly_2())
        result = loop.run_until_complete(start_dyn(loop))
        print(f'Got back {result}')
    finally:
        loop.close()


if __name__ == '__main__':
    main()
