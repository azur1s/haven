# ixc

`ixc` is a statically typed programming language and compiler, built specifically for DSP and audio plugin development.

> [!NOTE]
> This is very alpha and work in progress, codebase can be messay and bugs may arise, please report if you find one.

## Dependencies
- (Developer dependencies)
  - rust/cargo
- LLVM IR compiler
  - clang
  - opt + llc (untested)

## Usage
```shell
# compile to an executable
$ ixc program.ixc
$ ./output

# or, compile to a library
$ ixc lib.ixc --shared
$ clang host.c output.lib -o output

# use the help flag for more info
$ ixc -h
```