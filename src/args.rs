use std::path::PathBuf;
use clap::Parser;

#[derive(Parser, Debug)]
pub struct Args {
    /// The main input source file to compile
    #[arg(required = true)]
    pub input: PathBuf,

    /// The output file path for the compiled binary
    /// Defaults to `a.out` if not specified.
    #[arg(short, long, value_name = "OUTPUT", default_value = "output.exe")]
    pub output: PathBuf,

    /// The LLVM IR compiler to use (e.g., `clang`, `llc`, etc.)
    /// Defaults to `clang` if not specified.
    #[arg(short, long, value_name = "COMPILER", default_value = "clang")]
    pub compiler: String,

    /// LLVM IR compiler flags to pass to the compiler (e.g. `-O3 -Wall`, etc.)
    #[arg(short='F', long, value_name = "FLAGS", default_value = "-O3 -Wno-override-module")]
    pub compiler_flags: String,

    /// Compile as a shared dynamic library (.so / .dll / .dylib)
    #[arg(long, conflicts_with = "static_lib")]
    pub shared: bool,

    /// Compile as a static library (.a / .lib)
    #[arg(long, conflicts_with = "shared")]
    pub static_lib: bool,

    /// Stop compilation after generating LLVM IR (.ll)
    #[arg(long, conflicts_with = "keep_ir", conflicts_with = "emit_optimized_ir")]
    pub emit_ir: bool,

    /// Keep the generated LLVM IR file instead of cleaning it up after compilation
    #[arg(long, conflicts_with = "emit_ir")]
    pub keep_ir: bool,

    /// Emit back the optimized LLVM IR from the LLVM IR compiler (.opt.ll)
    /// Will follow the optimization flags passed to the LLVM IR compiler
    #[arg(long, conflicts_with = "emit_ir")]
    pub emit_optimized_ir: bool,
}