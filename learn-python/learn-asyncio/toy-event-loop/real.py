import asyncio


async def afunc_2():
    print("afunc_2: Starting")
    await asyncio.sleep(0)
    print("afunc_2: Stopping")


async def afunc_1():
    print("afunc_1: Starting")
    await afunc_2()
    print("afunc_1: Stopping")


def busy():
    print("busy: Starting")
    s = 0
    for i in range(10_000):
        for j in range(10_000):
            s += i + j
    print("busy: Ending")
    return s


try:
    loop = asyncio.new_event_loop()
    loop.create_task(afunc_1())
    loop.call_soon(busy)
    loop.run_forever()
except KeyboardInterrupt:
    pass
