# Julia Logging Macros

These are part of Julia's built-in `Logging` module (available by default, no import needed). They provide structured logging at different severity levels.

## Severity Levels
```julia
@debug "Detailed info for debugging"    # lowest priority, hidden by default
@info "General information"              # standard output
@warn "Something looks off"              # warnings
@error "Something failed"                # errors (but doesn't throw!)
```

## Structured Data

The macros support string interpolation and key-value pairs for structured data:
```julia
x = 42
@info "Processing value" x              # logs: Processing value | x = 42
@warn "Retrying request" attempt=3 max=5
@error "Failed to connect" host="api.example.com" status=500
```

## Important: `@error` Does Not Throw

A key distinction: **`@error` logs a message but does not throw an exception**. It's purely informational. If you want to both log and stop execution, you'd do:
```julia
@error "Critical failure" reason=err
throw(ErrorException("Critical failure"))
```

## Controlling Log Levels

You can control which levels are visible using `Logging.min_enabled_level` or environment variables. By default, `@debug` messages are suppressed. To see them:
```julia
using Logging
global_logger(ConsoleLogger(stderr, Logging.Debug))
```

Or for a specific block:
```julia
with_logger(ConsoleLogger(stderr, Logging.Debug)) do
    @debug "Now you can see me"
end
```