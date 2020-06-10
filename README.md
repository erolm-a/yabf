# Small Haskell BrainF*ck interpreter

Features (for a novel beginner):

- It avoids `String` and in general lists as much as possible due to their performance
- Uses `ST`, `IO` and the `State` monads (the latter via the `StateT`transfomer)
- Simply reads from stdin, spits to stdout. Any character outside the allowed grammar is ignored.
- Is self-contained. No other libraries involved.

# Possible extensions

1. Support "comments" (aka anything that is not a valid character is ignored)

2. Create a simple JIT

3. Any your ideas :)

# License

MIT
