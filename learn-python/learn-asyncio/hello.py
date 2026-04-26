import asyncio
from datetime import datetime


async def task1() -> str:
    print(f"{datetime.now().timestamp()}: Starting task1...")
    await asyncio.sleep(3)
    print(f"{datetime.now().timestamp()}: task1 completed.")
    return "task1 result"


async def task2() -> str:
    print(f"{datetime.now().timestamp()}: Starting task2...")
    await asyncio.sleep(0.5)
    print(f"{datetime.now().timestamp()}: task2 completed.")
    return "task2 result"


async def task3() -> str:
    print(f"{datetime.now().timestamp()}: Starting task3...")
    await asyncio.sleep(1)
    print(f"{datetime.now().timestamp()}: task3 completed.")
    return "task3 result"


async def main_1():
    print(f"{datetime.now().timestamp()} - Awaiting task1")
    t1 = await task1()
    # t1 = await asyncio.sleep(3, "task1 done")
    print(t1)

    print(f"{datetime.now().timestamp()} - Awaiting task2")
    t2 = await task2()
    # t2 = await asyncio.sleep(1, "task2 done")
    print(t2)

    print(f"{datetime.now().timestamp()} - Awaiting task3")
    t3 = await task3()
    # t3 = await asyncio.sleep(2, "task3 done")
    print(t3)


async def main():
    async with asyncio.TaskGroup() as tg:
        t1 = tg.create_task(task1())
        t2 = tg.create_task(task2())
        t3 = tg.create_task(task3())
        print("Tasks scheduled")
    print(t1.result())
    print(t2.result())
    print(t3.result())


asyncio.run(main())
