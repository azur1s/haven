# ixc

`ixc` is a statically typed programming language and compiler, built specifically for DSP and audio plugin development.

> [!IMPORTANT]
> This is work in progress. But I won't give up. At least without trying.

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