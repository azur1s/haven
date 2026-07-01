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

<!-- ## Modules

Split code across files with `import`:

```rust
import std/math              // whole module: symbols are visible UNQUALIFIED
import utils/helper          // local module, relative to THIS file's directory
import utils/mathx { scale } // selective: visible only QUALIFIED, as `mathx::scale`

proc main() i32 {
    let a: f64 = square(2.0f64);      // from `std/math`, unqualified
    let b: i32 = mathx::scale(3, 4);  // from the selective import, qualified
    ...
}
```

- `import a/b` pulls in every public item of module `a/b` under its bare name.
- `import a/b { x, y }` pulls in only `x` and `y`, reachable only as `b::x` / `b::y`.
- `std/...` paths resolve to the bundled standard library; any other path resolves
  to an `.ixc` file relative to the directory of the importing file.
- Qualified access (`b::x`) currently works for calls only; to use an imported
  *struct type*, import its module whole (types have no qualified spelling yet).

See `tests/mod_demo/` for a worked example. -->