# noir

`noir` is a statically typed programming language and compiler, built specifically for DSP and audio plugin development.

> [!NOTE]
> This is very alpha and work in progress, codebase can be messy and bugs may arise, please report if you find one.

## Dependencies
- (Developer dependencies)
  - rust/cargo
- LLVM IR compiler
  - clang
  - opt + llc (untested)

## Usage
```shell
# compile to an executable
$ noirc program.nr
$ ./output

# or, compile to a library
$ noirc lib.nr --shared
$ clang host.c output.lib -o output

# use the help flag for more info
$ noirc -h
```