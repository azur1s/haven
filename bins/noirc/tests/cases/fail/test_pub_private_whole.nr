// a whole-module `import` only brings a module's `pub` items into the qualifier
// namespace. a private item like std/alloc's `rt_alloc` never enters the `alloc::`
// namespace, so a qualified reference to it is unresolved. (the selective-import
// form reports "is private" directly; the whole-module form can only say the
// qualifier doesn't expose that name, see fail/test_pub_private_selective.nr.)

import std/alloc

@export
proc main() i32 {
    let p: *void = alloc::rt_alloc(8u64);
    return 0;
}

//@ error: is not imported from module qualifier 'alloc'
