# Event Loops

## Overview

Refs:

* [asyncio](https://docs.python.org/3/library/asyncio.html)
* [runners](https://docs.python.org/3/library/asyncio-runner.html)
* [tasks](https://docs.python.org/3/library/asyncio-task.html)

In the following program, with a bare `await`, the program's main event loop will suspend its execution until the awaited function returns.

```py
import asyncio

async def main():
  t = await asyncio.sleep(1, "done")
	print(t)
  
asyncio.run(main())
```

Will print "done" after 1 second. This is because the await causes the sleep function to be put on the scheduled queue, but there is nothing else on the ready queue. The main thread is suspended until the sleep task is ready after a second and run. Even if I had multiple such `await` calls, they will all run one after the other. Because until the first awaitable returns, I am not actually scheduling the second awaitable.

```py
import asyncio

async def main():
  t1 = await asyncio.sleep(3, "task 1 done")
  print(t1)
  t2 = await aysncio.sleep(1, "task 2 done")
  print(t2)
  t3 = await asyncio.sleep(2, "task 3 done")
  print(3)
  
asyncio.run(main())  
```

Will printout out "task 1 done", "task 2 done" and "task 3 done" in order after 3, 1, and 2 seconds respectively. If I want to schedule all the tasks one after the other I need to use task group (see hello.py) -

```py
async def main():
    async with asyncio.TaskGroup() as tg:
        t1 = tg.create_task(asyncio.sleep(3, "task 1 done"))
        t2 = tg.create_task(asyncio.sleep(1, "task 2 done"))
        t3 = tg.create_task(asyncio.sleep(2, "task 3 done"))
        # Tasks are implicitly awaited before the context ends
        print("Tasks scheduled")

    print(t1.result())
    print(t2.result())
    print(t3.result())
```

This will print out "Tasks scheduled" first. All three tasks will be put on the scheduled heap and then the main event loop will await for completion. Of course, the tasks will not neccessarily complete in the order they were scheduled. In the above example, task 2 gets done first, followed by task 3, and finally task 1.

Instead of using task groups, I can create task and invidually await their completion -

```py
async def main():
  # schedule both the tasks concurrently
  t1 = asyncio.create_task(fetch_data())
  t2 = asyncio.create_task(another_task())
  
  # do some other work that does not depend on the result of the tasks
  update_ui()
  
  # wait for the task when I need the results
  data = await t1
  update_ui()
  
  # wait for the remaining task
  await t2
```

📝 TODO: How are exceptions thrown by tasks handled? 

## Javascript

Loupe is a little visualisation to help you understand how JavaScript's call stack/event loop/callback queue interact with each other.

#### 2022-02-26

http://latentflip.com/loupe/

#lifelonglearning #systemsprogramming #videos #bookmarks 

## Python

### Conceptual Understanding

Lets say there is one user defined coroutine that calls another user defined coroutine which in turn calls a system coroutine that does some time consuming/blocking IO. There is also a user defined plain old python fuction f1 that does not depend on the output of the IO call.

```python
async def c2():
  ::: first block of code :::
  await asyncio.fn()  # This does some blocking IO
  ::: second block of code :::
    
async def c1():
  ::: first block of code :::
  await c2()
  ::: second block of code :::
    
def f1():
  ::: do something ::::
```

Here is what this looks like visually -

![asyncio-setup](./imgs/asyncio-setup.png)

When the program starts, I put `c1` and `f1` on the event loop's ready queue. This is how the story unfolds -

![asyncio-story](./imgs/asyncio-story.png)

See `//learn/learn-python/learn-asyncio/real.py` to see an example of this.

### Under the Hood

There are two main data structures at use here, a heap of scheduled callbacks (well their handles actually) and a FIFO queue of ready callbacks (again their handles). The event loop simply keeps calling these callbacks forever. Here is the pseudocode -

```
Main Loop
  Wait on select() which is a blocking call
  Get all the ready handles returned by select into the ready_queue
  Get all the scheduled handles that are ringing into the ready_queue
  Ready Loop
    Run each handle in the ready_queue until it is empty
  End Ready
  Next Main
End Main
```

The caller of the event loop can add callbacks directly to the ready queue or the scheduled heap using `call_*` family of methods. `call_later` and `call_at` add the callback to the scheduled heap, `call_soon` adds the callback directly on the ready queue. When the caller creates a client connection by calling `create_connection` or starts listening on a server socket by calling `create_server`, then the event loop uses its own methods as callbacks to either write to the socket or accept a client connection. It creates a handle out of one its own method, then registers this handle as a "data" with the selector. This way when there is an event with the associated fd, the event loop can get the "data" which is really the callback, and run it. 

So far we have seen that plain-old-python-functions are the ones that end up on the ready queue.    But the way to implement co-operative multitasking or greenlets is to somehow have coroutines in the event loop which can suspend their actions and then resume them again. How to make this happen? This is where Tasks and Futures come in. Here is the basic process - 

A coroutine is wrapped by a Task which puts its `.__step()` method on the ready queue immediately. Inside `Task.__step()`, it runs the coroutine till its first yield by calling `coro.send(None)`. Here is what happens inside the coroutine. It does whatever it can do that will not take a lot of time. Before it gets to the point where it has to do something time consuming, it creates a Future object and puts a special `futures._set_result_unless_cancelled` function into the ready queue. It has a reference to the newly created Future object. All it does is to set the future by calling `Future.set_result`. But more on that in a bit. So far this special function has been put at the back of the ready queue. The ready queue might have a bunch of other functions ahead of this special future function. And all this time the event loop is executing the `Task.__step` method. The coroutine then yields the newly created Future object. The `Task.__step` method takes this future and adds itself (well really `Task.__wakeup` which in turn will call `Task.__step` again) as a callback to this future. After that `Task.__step` ends. After all the other functions ahead of it are completed, eventually the `_set_results_unless_cancelled` function is called. Remember that al lit does is set the Future that it is holding, this is the same future that has the old `Task.__step` method as its callback. As soon as the future is set, it puts all its callbacks to the back of the ready queue, this means `Task.__step` is back in business. When it is finally dequed, it resumes the coroutine by calling `coro.send(None)` again. Now the coroutine goes and does the time consuming work. But it is ok to do this because all the other functions that were ready to be executed have already run. The coroutine completes its time consuming work. Whether it yields or returns is not entirely clear to me. But any rate it does not yield a future and `Task.__step` ends without adding itself back to any futures. To see an easy example see the `asyncio.sleep` implementation in `//cpython/Lib/asyncio/tasks.py::sleep`. 

To see a toy implementation of this entire setup see `//learn/learn-python/learn-asyncio/toy.py`. 

#### What happens in `run_once`?

1. Get all the ready events in the selector into the `ready` queue.

These events will be mostly [networking events](https://darkcoding.net/software/linux-what-can-you-epoll/). If there are items in the `ready` queue already, then make a quick `select` call (with timeout 0) and get all the events that have fired into the `ready` queue. Otherwise calculate after what time the first scheduled event is going to ring, and block on the `select` call until then. Of course if there are scheduled events already ringing, then timeout is still 0.

2. Get all the scheduled events that are ringing into the `ready` queue.

Scheduled callbacks have an absolute timestamp that says `when` it will ring. The event loop maintains an internal clock that gives out an absolute timestamp at any given instant. This part of the code just compares the current timestamp with the scheduled event's timestamp. Scheduled events are stored in a heap, so this part of the code just takes (and continues) to take items from the top of this heap and compare the timestamps.

3. Run the events in the `ready` queue.

By now, the `ready` queue has items that were already in it, items that the selector gave, and scheduled items that were ringing. This part of the code just goes through each item in the `ready` queue and calls the `.run()` method on the events, which are really handles of type `Handle` or `TimerHandle` which is just a wrapper around a plain old callable. The `handle.run()` method just calls the callable.

