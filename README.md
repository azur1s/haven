# haven

`haven` is a statically typed programming language and compiler, built specifically for DSP and audio plugin development.

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
$ havenc program.hv
$ ./output

# or, compile to a library
$ havenc lib.hv --shared
$ clang host.c output.lib -o output

# use the help flag for more info
$ havenc -h
```

## License
This project is dual-licensed under the MIT and Apache 2.0 licenses. See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.