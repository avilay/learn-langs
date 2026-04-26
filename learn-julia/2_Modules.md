# Julia Modules and Project Structure

## The `@main` Macro (Julia 1.11+)

The `@main` macro provides a clean entry point for Julia scripts, similar to `main()` in C. When you use `@main`, Julia automatically calls `Main.main(ARGS)` after the script finishes evaluating.

```julia
function (@main)(args)
    println("Hello from main!")
end
```

Run with:
```bash
julia script.jl
```

### Why `Main` module isn't explicitly declared

All top-level Julia code runs in an implicit module called `Main`:

```bash
$ julia -e 'println(@__MODULE__)'
Main
```

So when you define `(@main)(args)` at the top level, you're implicitly defining `Main.main(args)`.

### Comparison with manual approach

| Aspect | `@main` (Julia 1.11+) | Manual `if` check |
|--------|----------------------|-------------------|
| Auto-execution | Julia calls `main(ARGS)` automatically | You check `PROGRAM_FILE` yourself |
| Syntax | `function (@main)(args)` | `main() = ...; if ... main() end` |
| Args | Passed as parameter | Access `ARGS` global directly |

---

## Project Structure

Generate a standard Julia project:

```bash
julia -e 'using Pkg; Pkg.generate("MyApp")'
```

This creates:
```
MyApp/
├── Project.toml    # Package metadata & dependencies
└── src/
    └── MyApp.jl    # Top-level module
```

### The MyApp Example Project

Our example project structure:

```
MyApp/
├── Project.toml
├── src/
│   ├── MyApp.jl          # Top-level module
│   ├── utils/
│   │   └── StringUtils.jl
│   ├── FileUtils.jl
│   └── Stats.jl
├── main.jl               # Entry point with @main
└── duke_of_york.txt
```

#### src/utils/StringUtils.jl
```julia
module StringUtils

export countwords, countchars

countwords(text::String) = length(split(text))

countchars(text::String) = length(text)

end
```

#### src/FileUtils.jl
```julia
#=
FileUtils - File analysis utilities

Inclusion context:
  This module must be included from MyApp.jl AFTER StringUtils.jl.
  It expects StringUtils to be a sibling module (both children of MyApp).

Dependencies:
  - StringUtils: countwords, countchars
=#
module FileUtils

using ..StringUtils

export filestats

function filestats(filepath::String)
    content = read(filepath, String)
    (words=countwords(content), chars=countchars(content))
end

end
```

#### src/Stats.jl
```julia
module Stats

export average, total

average(nums) = sum(nums) / length(nums)

total(nums) = sum(nums)

end
```

#### src/MyApp.jl
```julia
module MyApp

include("utils/StringUtils.jl")
include("FileUtils.jl")
include("Stats.jl")

using .StringUtils
using .FileUtils
using .Stats

export countwords, countchars, filestats, average, total

end
```

#### main.jl
```julia
using MyApp

function (@main)(args)
    filepath = joinpath(@__DIR__, "duke_of_york.txt")

    # Using FileUtils (which internally uses StringUtils)
    stats = filestats(filepath)
    println("File stats: $(stats.words) words, $(stats.chars) chars")

    # Using StringUtils directly
    text = "Julia is fun"
    println("'$text' has $(countwords(text)) words")

    # Using Stats
    numbers = [10, 20, 30, 40]
    println("Numbers $numbers: total=$(total(numbers)), average=$(average(numbers))")
end
```

#### Running the project
```bash
cd MyApp
julia --project=. main.jl
```

Output:
```
File stats: 28 words, 121 chars
'Julia is fun' has 3 words
Numbers [10, 20, 30, 40]: total=100, average=25.0
```

---

## `include` vs `using`

These serve different purposes:

| Statement | What it does |
|-----------|--------------|
| `include("X.jl")` | Textually inserts the file, *defining* the module |
| `using .X` | Brings exported names from an *already existing* module into scope |

### You need both

```julia
module MyApp
    include("StringUtils.jl")  # Creates MyApp.StringUtils
    using .StringUtils         # Brings exports into MyApp's scope
    export countwords          # Re-export for convenience
end
```

### Why `include` is required

Without `include`, the module doesn't exist:

```bash
$ julia -e '
module MyApp
    using .StringUtils  # ERROR: StringUtils not defined
end
'
ERROR: UndefVarError: `StringUtils` not defined in `Main.MyApp`
```

### Why `using` after `include`

The `include` defines the module, but its exports aren't automatically available in the parent scope. You need `using` to bring them in, and then `export` to re-export them for users of the parent module.

Without re-exporting, users must access submodules explicitly:
```julia
using MyApp.StringUtils  # Must specify submodule
countwords("hello")
```

With re-exporting:
```julia
using MyApp  # Everything available from top-level
countwords("hello")
```

---

## Module Hierarchy vs Filesystem

The module hierarchy is determined by where `include` is called, not where files live on disk.

```julia
# In MyApp.jl
include("utils/StringUtils.jl")  # File in subdirectory
include("FileUtils.jl")          # File in same directory
```

Both become direct children of `MyApp`, making them siblings:

```
Module hierarchy:          Filesystem:
MyApp                      src/
├── StringUtils            ├── MyApp.jl
├── FileUtils              ├── utils/
└── Stats                  │   └── StringUtils.jl
                           ├── FileUtils.jl
                           └── Stats.jl
```

So `FileUtils.jl` uses `using ..StringUtils` regardless of where `StringUtils.jl` lives on disk.

### Order matters

Submodules can only `use` siblings that were `include`-ed before them:

```julia
# In MyApp.jl
include("StringUtils.jl")  # 1. Creates MyApp.StringUtils
include("FileUtils.jl")    # 2. Can use ..StringUtils (exists)
include("Stats.jl")        # 3. Could use ..StringUtils or ..FileUtils
```

Reversing the order would cause an error.

### Comparison with Python

Python's import system is more explicit - each file specifies the full path to its dependencies:

```python
from myapp.utils.string_utils import countwords  # Explicit path
```

Julia's approach creates implicit coupling - a file must know how it will be included. This is a known tradeoff in Julia's design, prioritizing compilation simplicity over import explicitness.

**Best practice**: Document the expected inclusion context in files that depend on siblings.

---

## Packages vs Submodules

Why can `main.jl` just do `using MyApp` without `include`?

Because `MyApp` is a **package** (defined by `Project.toml`), not just a module file.

| Type | How it's found | Example |
|------|----------------|---------|
| **Package** | Julia looks it up via `Project.toml` | `using MyApp` |
| **Submodule** | Must be `include`-ed first | `include("X.jl"); using .X` |

When you run `julia --project=.`:
1. Julia reads `Project.toml`
2. Sees `name = "MyApp"`
3. Automatically finds `src/MyApp.jl`

```bash
$ julia --project=. -e 'using MyApp; println(pathof(MyApp))'
/path/to/MyApp/src/MyApp.jl
```

---

## `using` vs `import`

| Syntax | What it does |
|--------|--------------|
| `using Module` | Brings all exported names into scope directly |
| `import Module` | Only brings the module name; access via `Module.func()` |
| `import Module: foo, bar` | Brings specific names into scope |

### Example

```julia
module Demo
    export greet
    greet() = "hello"
    secret() = "hidden"
end

using .Demo
greet()        # Works - exported
# secret()     # ERROR - not exported
Demo.secret()  # Works - explicit access
```

### Key difference: extending methods

To add methods to a function from another module, you must use `import`:

```julia
module Demo
    export greet
    greet() = "default"
end

# With `using`, this creates a NEW function (usually not what you want):
# using .Demo
# greet(name) = "hello $name"  # Creates Main.greet, not Demo.greet

# With `import`, this EXTENDS the existing function:
import .Demo: greet
greet(name::String) = "hello $name"  # Adds method to Demo.greet

greet()         # "default"
greet("world")  # "hello world"
```

### When to use which

| When to use | Keyword |
|-------------|---------|
| Just calling functions | `using` (simpler) |
| Extending functions with new methods | `import Module: func` (required) |
| Prefer explicit namespacing | `import Module` |

---

## Selective Imports

You can import only specific names:

```julia
using Dates: Date, today  # Only these two names
```

vs:

```julia
using Dates  # All ~50+ exported names
```

### Benefits of selective imports

1. **Avoid namespace pollution** - don't clutter scope with unused names
2. **Clarity** - explicit about which names come from where
3. **Avoid conflicts** - when multiple modules export the same name

```julia
using ModuleA: foo
using ModuleB: bar  # Both might export `baz`, but we avoid conflict
```

### Example

```bash
$ julia -e '
using Dates
println(today())   # Works
println(now())     # Works
println(Monday)    # Works (day constant)
'
2025-12-11
2025-12-11T09:43:19.761
1
```

```bash
$ julia -e '
using Dates: Date, today
println(today())   # Works
# println(now())   # ERROR - not imported
# println(Monday)  # ERROR - not imported
'
2025-12-11
```

---

## Summary Table

| Concept | Syntax | Purpose |
|---------|--------|---------|
| Entry point | `function (@main)(args)` | Script entry point (Julia 1.11+) |
| Define submodule | `include("X.jl")` | Textually insert and define module |
| Access child | `using .X` | Single dot = child of current module |
| Access sibling | `using ..X` | Double dot = go up, then find X |
| All exports | `using Module` | Bring all exported names into scope |
| Specific exports | `using Module: a, b` | Bring only specified names |
| Module name only | `import Module` | Access via `Module.func()` |
| Extend functions | `import Module: func` | Required to add methods |
| Re-export | `export name` | Make name available to users |
