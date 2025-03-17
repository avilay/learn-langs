"""
The asyncio.run() starts an event loop with the main1 function. The event loop is running on the main thread. And it starts the main1 function on that thread. The main1 function creates two tasks but these tasks are not started yet by the event loop. The main1 function then yields waiting for some event. This is encapsulated by calling the await function with the event it is waiting on. The control goes back to the event loop who will now opportunistically start all the pending tasks in order of creation. It will start with task1 and run it until it yields (awaits) on some event. After the control passes back to the event loop, even if the main1 function is ready to resume, it will not get its chance. The event loop is now determined to start all the pending tasks, so it goes on to task2 and runs that until the first yield. Now that all pending tasks have been started, it will execute the function whose event fires first to its next yield and so on. Eventually the main1 function must await for the tasks to complete, otherwise it will just exit before the other tasks might be done.

The workflow of starting tasks and then awiating for them is encapsulated in the asyncio.gather function.
"""

import asyncio
import time

def fib(n):
    if n <= 2:
        return 1
    return fib(n-1) + fib(n-2)

async def func1(arg):
    print(f"[{time.strftime('%X')}] func1: Starting")
    x = fib(30)
    print(f"[{time.strftime('%X')}] func1: fib(30)={x}")
    await asyncio.sleep(0.5)
    print(f"[{time.strftime('%X')}] func1: {arg}")

async def func2(arg):
    print(f"[{time.strftime('%X')}] func2: Starting")
    x = fib(35)
    print(f"[{time.strftime('%X')}] func2: fib(35)={x}")
    await asyncio.sleep(1)
    print(f"[{time.strftime('%X')}] func2: Doing some other work")
    time.sleep(3)
    print(f"[{time.strftime('%X')}] func2: {arg}")

async def main1():
    print(f"[{time.strftime('%X')}] main: Starting")

    # These tasks will be started opportunistically
    task1 = asyncio.create_task(func1("hello"))
    task2 = asyncio.create_task(func2("world"))

    print(f"[{time.strftime('%X')}] main: Doing heavy duty async work")
    await asyncio.sleep(0.5)
    # At this point, the main thread is waiting for an event to happen
    # The event loop will start task1 even though I have not called await on it!
    # It will run it to its first yield.
    # If the main loop is still waiting, then the event loop will start task2 and
    # run that to its first yield.
    # After that all three functions are waiting for events. The func whose event
    # fires first grabs the main thread and executes until its next yield and so on.
    # It is entirely possible that task1 (i.e., func1) completes even before its await
    # is ever called. At that point the await will return immeidately.

    print(f"[{time.strftime('%X')}] main: Doing some more heavy duty async work")
    await asyncio.sleep(0.5)
    # await task1
    # await task2

    print("main: Done")

async def main2():
    print(f"[{time.strftime('%X')}] main: Starting")

    # Will automatically wrap func1 and func2 in tasks
    await asyncio.gather(
        func1("hello"),
        func2("world")
    )

asyncio.run(main1())
