# Tools

## Profiling

`gprof`. Compile as `gcc -pg` which will generate perf counters in a file called *gmon.out*. Then you somehow open this with `gprof`, or do you run your program with `gprof`? Anyway, this is how you get perf counters. This only profiles the direct executable, not the libraries. If I want to profile a library I need to copy it into the executable's code base and run `gprof`. 

## Memory Leaks

`valgrind`. Running the executable with `valgrind myprog` will generate a stacktrace of problematic portions of the code where there might be memory leaks.

## Code Coverage

`gcov`. Compiling with `gcc -f profile-arcs -f test-coverage -O0`. Now when the code runs, *.gcda* and *.gcno* files will be generated which can be read by `gcov` to give CC stats.

 

