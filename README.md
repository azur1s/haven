<samp>

# ichor

ichor is a ML-like language hoping to be used for writing backends. Target JavaScript.

I know JavaScript isn't supposed to be on backend, but come on, it works.

> [!WARNING]
> Work in progress.

## Progress

This project will be using [this versioning](https://pridever.org/). We are at `0.0.0`

The next goal is to be able to write a Discord bot as a dogfooding process, and after that maybe it'll be `0.1.0`

- Language
    - (X) Basic expressions
    - ( ) Type inference (it works but will never be completed)
    - ( ) Optimization (I will be focusing on making everything works)
    - (-) File includes

        There is `use` syntax for including files relative to the file being compiled and also for including `std` stuff

        ```sml
        use std/io
        use ./foo (* everything starts with ./ will be relative *)
        ```

        No library management yet and/or build system so there's only `std` that isn't relative.

    - ( ) Match & Pattern matching at function
    - ( ) Mutability (everything is call by value at the moment)

        Will be copying OCaml

    - ( ) Custom operators

        Should be easy since we use pratt parser and we can probably just add new operators at runtime :clueless:

    - (X) Custom type definitions/alias

        ```
        type foo = int * int (* DONE *)
        let a = foo (1, 2) (* DONE *)

        (* TODO *)
        let bar = bar int
        let baz = a int | b bool
        let qux = { a : int, b : bool }
        ```

    - ( ) Methods for said custom types

        Probably do something like Rust where it's `.0`, `.1` ... for `T * U * ...` types

    - ( ) Traits?
    - ( ) Async/await or Promises

        Probably something like Firefly from Ahnfelt where await are automatically inserted (I have not yet to learn the wizardry behind it)

    - ( ) JS interop in both directions

        There is `__js__`, `__js_method__`, `__js_field__` and `__inline__`

        Probably better to use something like Firefly (again) where its a `JsValue` type instead of multiple intrinsic functions returning equivalent of `any` type

        For libraries: either parse .d.ts or port the types to ichor
        or ... just rewrite everything in ichor

- Standard Libraries
    - (-) [std/num.ich](std/num.ich) Numbers (like 3 functions)

        Just port a lot of `Math.*` functions (I'll deal with it later)

    - (-) [std/io.ich](std/io.ich) IO (basic amount + no formatting + no file IO)
    - (-) [std/io.ich](std/list.ich) List

        `map`, `iter` and `fold_left` are implemented

        The goal is to be equivalent to OCaml's List module

    - (-) [std/basehttp.ich](std/basehttp.ich) http (proof-of-concept instead of being usable)

        I'll revisit after finishing SwE final (after end of Jan)

    - ( ) Literally everything
- Examples & Dogfooding
    - (-) Showcase basic language features
    - (X) Fibonacci (of course)
    - ( ) Discord

        Both waiting for async to be done & basic Discord library reimplementation OR interop with something like eris.js

    - (-) HTTP server (as good as the http stdlib)