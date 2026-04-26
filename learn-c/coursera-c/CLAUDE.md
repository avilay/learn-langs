# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a C learning project from Coursera, using C17 standard. The codebase demonstrates fundamental C programming concepts including custom memory allocation.

## Build System

This project uses CMake with a minimum version requirement of 4.0.

### Building the Project

```bash
# Configure and build (from project root)
cd build
cmake ..
make

# Run the executable
./coursera_c
```

### Clean Build

```bash
cd build
rm -rf *
cmake ..
make
```

## Code Architecture

### Memory Allocation Module (`alloc.c` / `alloc.h`)

A simple custom memory allocator demonstrating C module design patterns:

- **Static module state**: Uses `static` variables (`buf` and `next_free`) to maintain module-level state that's globally scoped in lifetime but not visible outside the module
- **Fixed-size buffer**: Pre-allocated 1024-byte buffer for all allocations
- **Linear allocation**: Simple bump-pointer allocator that moves `next_free` forward
- **Stack-like deallocation**: `free_mem()` resets `next_free` to the given pointer, effectively freeing all allocations made after that point

This is a teaching implementation, not production-ready code. It demonstrates:
- Module encapsulation using `static` variables
- Header/implementation file separation
- Bounds checking in allocation/deallocation

### Main Entry Point (`main.c`)

Currently contains a minimal "Hello, World!" program. This serves as the entry point where memory allocation examples would be integrated.

## CMake Configuration

The project is configured to:
- Use C17 standard (`CMAKE_C_STANDARD 17`)
- Build an executable named `coursera_c`
- Compile `main.c`, `alloc.c`, and include `alloc.h`
