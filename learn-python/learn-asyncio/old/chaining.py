import asyncio


async def outer():
    print('in outer')
    print('waiting for result1')
    result1 = await phase1()
    print('waiting for result2')
    result2 = await phase2()
    return result1, result2


async def phase1():
    print('in phase1')
    return 'result1'


async def phase2():
    print('in phase2')
    return 'result2'


def main():
    loop = asyncio.get_event_loop()
    try:
        result = loop.run_until_complete(outer())
        print(f'Got back {result}')
    finally:
        loop.close()

