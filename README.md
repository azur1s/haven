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

## Directory Structure
```
bins/
├── havenc/         # compiler
└── havendoc/       # mdBook documentation generator
crates/
├── haven_back/     # backend-related code (LLVM IR, ABI)
├── haven_common/   # common code & types (AST, Diagnostics, memory layout, etc.)
├── haven_front/    # frontend-related code (lexer, parser, modules)
└── haven_mid/      # middle-end code (type checking, semantic analysis, etc.)
crt/
└── rt.c            # linked C runtime for the compiler
extensions/
└── vscode/         # VSCode extension for syntax highlighting
std/                # haven standard library
└── ...
```

## License
This project is dual-licensed under the MIT and Apache 2.0 licenses. See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.