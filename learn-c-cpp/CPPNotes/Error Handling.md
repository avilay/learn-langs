# Error Reporting & Handling

## C

Due to lack of exceptions there are three different ways to indicate errors.

1. Use `errno`.
2. Have a special error object that is either directly returned, or is an out param. The actual return value will have to be an out param or direct return respectively.
3. Have a response structure that includes success/error status in it.

### Errno

The basic idea is that `errno` is a thread-safe global variable that can be set to a strictly positive integer if an error occurs. The caller of the erroneous function should check `errno` to see the specific error. The return type of this function should be an `int`. A return value of $-1$ indicates an error and the caller should check `errno`. If the conceptual return type of this function is an int, it can be bundled with return value as long as $-1$ is not a valid return value. If not, then the conceptual return struct should be an out praam. The actual return will be $0$ in case of success and $-1$ in case of error.

On my current system (Arch Linux on Intel x86) there are around 135 different error codes defined. To get a human readable description of the error code use either `perror` or `strerror`. The main header file for using these functions as well defined  error codes is `<error.h>`. On my system, I followed it and found the error codes defined in the following headers -

* `/usr/include/asm-gemeric/errno-base.h`
* `/usr/include/asm-generic/errno.h`
* `/usr/include/bits/error.h`

As part of the `moreutils` package I can get an `errno` CLI utility that gives out all of this information more easily. 

* `errno --list` to list all defined errors from all the header files.
* `errno --search <word>` to search.





