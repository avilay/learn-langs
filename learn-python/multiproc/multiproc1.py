"""
This script shows how to use the Pool class. This is convenient as the Python 
runtime will take care of forking processes for all the tasks. Calling Pool()
without any args or with `None` will create as many processes as the number of 
CPUs in the system.
"""

import os
import time
from datetime import datetime
import random

from functools import partial
from multiprocessing.pool import Pool

from cprint import cprint

MAP = dict(
    one=1,
    two=2,
    three=3,
    four=4,
    five=5,
    six=6,
    seven=7,
    eight=8,
    nine=9,
    ten=10,
    eleven=11,
    twelve=12,
)


def dprint(rank, text):
    pid = os.getpid()
    now = datetime.now().strftime("%m-%d %I:%M:%S:%f %p")
    cprint(rank, f"[{rank}]({pid}){now} - {text}")


def task_1(arg):
    # cprint(0, '\t\tStarting task with {}'.format(arg))
    dprint(0, f"Starting task_1 with {arg}")
    time.sleep(random.randint(1, 10))
    # print('\t\tEnding task with {}'.format(arg))
    dprint(0, f"Ending task_1 for {arg}.")
    return MAP[arg]


def task_2(arg1, arg2):
    # print('\t\tStarting task with {} and {}'.format(arg1, arg2))
    dprint(1, f"Starting task_2 with {arg1}, {arg2}")
    time.sleep(5)
    # print('\t\tEnding task with {} and {}'.format(arg1, arg2))
    dprint(1, f"Ending task_2 for {arg1}, {arg2}")
    return MAP[arg1] + MAP[arg2]


def success(ret):
    print("Got back {}".format(ret))


def error(exception):
    print("Error {}".format(exception))


def learn_apply():
    """
    Pool.apply is a blocking call that forks off the specified function and all the args in a separate process. For most
    purposes this is useless.
    """
    with Pool(processes=2) as pool:
        result = pool.apply(task_1, ("one",))
        print(f"Task completed! {result}")
        result = pool.apply(task_2, ("nine", "twelve"))
        print(f"Task completed! {result}")


def learn_apply_async():
    """
    This is an async version of apply. Useful when I want to call different functions with different arguments. The call
    returns a promise. As usual the call to get the result of the promise is blocking.
    """
    with Pool(processes=2) as pool:
        result_1 = pool.apply_async(task_1, ("one",))
        result_2 = pool.apply_async(task_2, ("nine", "twelve"))

        # Because I have only two processes in my pool, this task will not be started until the previous two complete.
        result_3 = pool.apply_async(task_1, ("three",))

        # Now block
        ans1 = result_1.get()
        print(f"Task 1 completed {ans1}")

        ans2 = result_2.get()
        print(f"Task 2 completed {ans2}")

        ans3 = result_3.get()
        print(f"Task 3 completed {ans3}")


def learn_map():
    """
    Pool.map takes in a function and a sequence of values. It starts a process for each element in the sequence. These
    processes are started in parallel, but the call itself will block untill all the processes return. The return values
    are those returned by the function. There are two constraints to this method -
      * Each worker executes the same function, but cannot different workers execute different funcs.
      * The function can only take in a single parameter. Of course I can always curry a multi param function using
        partial.
    """
    with Pool(processes=2) as pool:
        results = pool.map(task_1, ("one", "two", "three"))
        print("Results obtained.")

    for i, result in enumerate(results):
        print(f"Task_1 {i} returned {result}")

    task_2_with_twelve = partial(task_2, "twelve")
    with Pool(processes=2) as pool:
        results = pool.map(task_2_with_twelve, ("one", "two", "three"))
        print("Results obtained.")
    for i, result in enumerate(results):
        print(f"Task_1 {i} returned {result}")


def learn_map_async():
    """
    Very similar to map, except the call returns immediately with a promise.
    """
    with Pool(processes=2) as pool:
        promises = pool.map_async(task_1, ("one", "two", "three"))
        print("Promises obtained.")
        results = promises.get()

    for i, result in enumerate(results):
        print(f"Task_1 {i} returned {result}")


def learn_starmap():
    """
    Very similar to map except I can use a function that accepts multiple params. Just like map this is also a
    blocking call.
    """
    with Pool(processes=2) as pool:
        results = pool.starmap(task_2, [("one", "two"), ("three", "five")])
        print("Results obtained.")

    for i, result in enumerate(results):
        print(f"Task_1 {i} returned {result}")


def learn_starmap_async():
    """
    Very similar to starmap except that it immediately returns a promise.
    """
    with Pool(processes=2) as pool:
        promises = pool.starmap_async(task_2, [("one", "two"), ("three", "five")])
        print("Promises obtained.")
        results = promises.get()

    for i, result in enumerate(results):
        print(f"Task_1 {i} returned {result}")


if __name__ == "__main__":
    # learn_apply()
    # learn_apply_async()
    # learn_map()
    # learn_map_async()
    # learn_starmap()
    learn_starmap_async()
