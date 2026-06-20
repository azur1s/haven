fn main() {
    println!("cargo:rerun-if-changed=crt/rt.c");

    cc::Build::new()
        .file("crt/rt.c")
        .opt_level(3)
        .compile("runtime");
}