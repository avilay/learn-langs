# Polling

## File Control

The usual APIs `send` and `recv` are used for IO calls on file descriptors. These are blocking calls, `recv` will block until there is data to read, and `send` will block until the file descriptor is ready to send the data. Depending on the file descriptor I can ask the OS to poll for read/write events on it.

```c
int fcntl(int fildes, int cmd, ...);
```

The general usage of this API is to pass the file descriptor as the first arg, then the command we want to execute on this fd, and then a list of pass through args that will be used by the given command. To setup an fd for non-blocking calls do the following -

```c
fcntl(fd, F_SETFL, O_NONBLOCK);
```

Of course if the fd does not support non-blocking ops this will not work (according to the spec, the behavior is undefined). Now when I `read` from this fd, if there is no data it will immediately return $-1$ with the `errno` set to `EAGAIN` or `EWOULDBLOCK`. At this point my code can do some other stuff and check for any data in the next tick cycle/event loop/what have you.

```c
while (!terminate) {
    int num_bytes = read(...);
    if (num_bytes != -1) {
        // do something with the incoming data.
    } else {
        if (errno != EAGAIN) {
            // handle error
        }
        // run the rest of the simulation loop with no new data
    }
}
```

## Poll

A common pattern in networking is to monitor a bunch of fds and sleep until at least one of them is ready to use. `poll()` is used for this.

```c
int poll(struct pollfd fds[], nfds_t nfds, int timeout);
```

I pass a bunch of fds that I want to monitor to the `poll()` function along with a timeout value. This call will block until one of the fds is ready. If no fds are ready, then the poll will return after timeout milliseconds. The `pollfd` struct is used to specify what kind of events I want to monitor for. It also serves as an outparam wherein the events that actually did occur are specified when the call returns.

```c
struct pollfd {
    int fd;
    short events;  // specify what events to monitor for
    short revents;  // contains events that actually occured
};
```

The events are bitmasks constructed using bitwise-|. The two most common events are `POLLIN` and `POLLOUT` for when the fd is ready to read and write respectively.

See `poll_demo.c` for a full example.





