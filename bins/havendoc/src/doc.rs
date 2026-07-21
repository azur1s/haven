//! `havendoc`: turn `.hv` source into mdBook-ready Markdown.
//!
//! The lexer discards comments as padding (see `parse::lexer`), so doc text
//! never reaches the AST. Rather than thread doc tokens through the whole
//! grammar, it parses each file for *real* signatures (generics, types,
//! attributes all rendered via the AST's `Display` impls) and recovers the
//! `///` doc bodies straight from the source bytes, keyed by each item's `Span`.
//! Same result as attaching docs to the AST.
//!
//! Output layout, given `havendoc std -o docs`:
//! ```text
//! docs/
//!   book.toml
//!   src/
//!     SUMMARY.md
//!     std/alloc.md
//!     std/dsp/osc.md
//!     ...
//! ```

use std::path::{Path, PathBuf};

use bumpalo::Bump;

use crate::DocArgs;
use haven_common::ast::{TopLevel, TopLevelNode, Type};
use haven_front::parse;

/// Entry point for the `doc` subcommand. Returns `Err(())` if nothing could be
/// documented (bad path, or every file failed to parse); individual per-file
/// failures are reported and skipped.
pub fn generate(args: &DocArgs) -> Result<(), ()> {
    let files = collect_hv_files(&args.inputs);
    if files.is_empty() {
        eprintln!("havendoc: no .hv files found in {:?}", args.inputs);
        return Err(());
    }

    let src_dir = args.out.join("src");
    if let Err(e) = std::fs::create_dir_all(&src_dir) {
        eprintln!("havendoc: cannot create {}: {}", src_dir.display(), e);
        return Err(());
    }

    // (module title, page path relative to src/) for the SUMMARY, in the order
    // pages were emitted.
    let mut pages: Vec<(String, PathBuf)> = Vec::new();

    for (strip_base, file) in &files {
        let title = module_title(strip_base, file);
        let rel_md = title_to_rel_path(&title);
        let page_path = src_dir.join(&rel_md);

        let markdown = match render_file(&title, file) {
            Ok(md) => md,
            Err(()) => {
                eprintln!("havendoc: skipping {} (failed to parse)", file.display());
                continue;
            }
        };

        if let Some(parent) = page_path.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                eprintln!("havendoc: cannot create {}: {}", parent.display(), e);
                continue;
            }
        }
        if let Err(e) = std::fs::write(&page_path, markdown) {
            eprintln!("havendoc: cannot write {}: {}", page_path.display(), e);
            continue;
        }
        pages.push((title, rel_md));
    }

    if pages.is_empty() {
        eprintln!("havendoc: no pages generated");
        return Err(());
    }

    // Stable, human-friendly ordering in the sidebar.
    pages.sort_by(|a, b| a.0.cmp(&b.0));

    write_summary(&src_dir, &pages)?;
    write_book_toml(&args.out, "haven std")?;
    write_theme(&args.out)?;
    write_file(&args.out.join(".gitignore"), GITIGNORE.as_bytes())?;

    println!(
        "havendoc: wrote {} page(s) to {}",
        pages.len(),
        args.out.display()
    );
    Ok(())
}

/// Recursively gather `.hv` files from each input. Returns `(strip_base, file)`
/// pairs, where `strip_base` is the path prefix stripped off `file` to form its
/// module title. For a directory input it's the directory's *parent*, so the
/// directory name is kept (`std/` -> `std/dsp/osc`); for a file input it's the
/// file's parent, so a lone `foo/bar.hv` titles as just `bar`.
fn collect_hv_files(inputs: &[PathBuf]) -> Vec<(PathBuf, PathBuf)> {
    let mut out = Vec::new();
    for input in inputs {
        if input.is_dir() {
            let base = input.parent().unwrap_or_else(|| Path::new("")).to_path_buf();
            walk_dir(&base, input, &mut out);
        } else if is_hv(input) {
            let base = input.parent().unwrap_or_else(|| Path::new("")).to_path_buf();
            out.push((base, input.clone()));
        } else {
            eprintln!("havendoc: skipping {} (not a .hv file)", input.display());
        }
    }
    out
}

fn walk_dir(base: &Path, dir: &Path, out: &mut Vec<(PathBuf, PathBuf)>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("havendoc: cannot read {}: {}", dir.display(), e);
            return;
        }
    };
    // Collect + sort so output is deterministic across filesystems.
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok().map(|e| e.path())).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            walk_dir(base, &path, out);
        } else if is_hv(&path) {
            out.push((base.to_path_buf(), path));
        }
    }
}

fn is_hv(p: &Path) -> bool {
    p.extension().and_then(|e| e.to_str()) == Some("hv")
}

/// Strip `strip_base` off `file` and drop the extension to form a module title:
/// `std/dsp/osc.hv` with base `` -> `std/dsp/osc`; `foo/bar.hv` with base `foo`
/// -> `bar`. Falls back to the full path when `file` isn't under `strip_base`.
fn module_title(strip_base: &Path, file: &Path) -> String {
    let rel = file.strip_prefix(strip_base).unwrap_or(file);
    let rel = rel.with_extension("");
    rel.to_string_lossy().replace('\\', "/")
}

fn title_to_rel_path(title: &str) -> PathBuf {
    PathBuf::from(format!("{}.md", title))
}

// ---------------------------------------------------------------------------
// Per-file rendering
// ---------------------------------------------------------------------------

fn render_file(title: &str, file: &Path) -> Result<String, ()> {
    let src = std::fs::read_to_string(file).map_err(|e| {
        eprintln!("havendoc: cannot read {}: {}", file.display(), e);
    })?;

    // Parse into real signatures. A fresh arena per file is fine: everything we
    // keep (the rendered strings) is owned by the time it drops.
    let arena = Bump::new();
    let key: &str = arena.alloc_str(&file.to_string_lossy());
    let src_ref: &str = arena.alloc_str(&src);

    let (tokens, lex_errs) = parse::lex(key, src_ref);
    let tokens = match tokens {
        Some(t) if lex_errs.is_empty() => t,
        _ => return Err(()),
    };
    let tokens = arena.alloc_slice_fill_iter(tokens);
    let (parsed, parse_errs) = parse::parse(key.to_string(), src_ref.len(), tokens);
    let (_imports, items) = match parsed {
        Some(pi) if parse_errs.is_empty() => pi,
        _ => return Err(()),
    };

    let lines = LineIndex::new(&src);

    let mut md = String::new();
    md.push_str(&format!("# `{}`\n\n", title));

    // Module-level doc: a leading `///` block separated from the first item by a
    // blank line (mirrors the intent of Rust's `//!`).
    if let Some(doc) = lines.module_doc() {
        md.push_str(&doc);
        md.push_str("\n\n");
    }

    if items.is_empty() {
        md.push_str("_This module exposes no documented items._\n");
        return Ok(md);
    }

    for item in &items {
        // docs describe a module's public surface; private (non-`pub`) items are
        // implementation details and are omitted entirely.
        if !item_is_pub(&item.value) {
            continue;
        }
        render_item(&mut md, item, &src, &lines);
    }

    Ok(md)
}

/// Whether a top-level item is `pub` (part of the module's public API).
fn item_is_pub(node: &TopLevelNode) -> bool {
    match node {
        TopLevelNode::Function { is_pub, .. }
        | TopLevelNode::Extern { is_pub, .. }
        | TopLevelNode::Struct { is_pub, .. }
        | TopLevelNode::Global { is_pub, .. } => *is_pub,
    }
}

fn render_item(md: &mut String, item: &TopLevel, src: &str, lines: &LineIndex) {
    let name = item_name(&item.value);
    let signature = signature(&item.value, src, item.span.start, item.span.end);
    let doc = lines.doc_above(item.span.start);

    md.push_str(&format!("## `{}`\n\n", name));
    md.push_str("```hv\n");
    md.push_str(&signature);
    md.push_str("\n```\n\n");
    if let Some(doc) = doc {
        md.push_str(&doc);
        md.push_str("\n\n");
    }
}

fn item_name<'a>(node: &TopLevelNode<'a>) -> &'a str {
    match node {
        TopLevelNode::Function { name, .. }
        | TopLevelNode::Extern { name, .. }
        | TopLevelNode::Struct { name, .. }
        | TopLevelNode::Global { name, .. } => name,
    }
}

/// Render a declaration's signature without its body. Types, generics and
/// attributes all reuse the AST `Display` impls so the output tracks the real
/// grammar. `start`/`end` bound the item in `src`, used to recover per-field
/// trailing doc comments on structs.
fn signature(node: &TopLevelNode, src: &str, start: usize, end: usize) -> String {
    match node {
        TopLevelNode::Function { name, attributes, generics, params, return_type, .. } => {
            let mut s = attr_prefix(attributes);
            s.push_str(&format!(
                "proc {}{}({}) {}",
                name,
                fmt_generics(generics),
                fmt_params(params),
                return_type
            ));
            s
        }
        TopLevelNode::Extern { name, attributes, generics, params, return_type, .. } => {
            let mut s = attr_prefix(attributes);
            s.push_str(&format!(
                "extern {}{}({}) {};",
                name,
                fmt_generics(generics),
                fmt_params(params),
                return_type
            ));
            s
        }
        TopLevelNode::Struct { name, attributes, generics, fields, .. } => {
            let mut s = attr_prefix(attributes);
            s.push_str(&format!("struct {}{} {{\n", name, fmt_generics(generics)));
            let field_docs = struct_field_docs(src, start, end);
            for (fname, fty) in fields {
                let doc = field_docs
                    .iter()
                    .find(|(n, _)| n == fname)
                    .map(|(_, d)| format!("  /// {}", d))
                    .unwrap_or_default();
                s.push_str(&format!("    {}: {},{}\n", fname, fty, doc));
            }
            s.push('}');
            s
        }
        TopLevelNode::Global { name, attributes, ty, value, .. } => {
            let mut s = attr_prefix(attributes);
            s.push_str(&format!("const {}: {} = {};", name, ty, value.value));
            s
        }
    }
}

fn attr_prefix(attributes: &[haven_common::ast::Attribute]) -> String {
    if attributes.is_empty() {
        String::new()
    } else {
        attributes
            .iter()
            .map(|a| a.value.to_string())
            .collect::<Vec<_>>()
            .join("\n")
            + "\n"
    }
}

fn fmt_generics(generics: &[haven_common::ast::GenericParam]) -> String {
    if generics.is_empty() {
        String::new()
    } else {
        format!(
            "<{}>",
            generics.iter().map(|g| g.to_string()).collect::<Vec<_>>().join(", ")
        )
    }
}

fn fmt_params(params: &[(&str, Type)]) -> String {
    params
        .iter()
        .map(|(n, t)| format!("{}: {}", n, t))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Recover `field: ... /// doc` trailing comments from a struct's source text.
/// The AST drops these (they're padding), and struct fields carry no per-field
/// span, so we scan the item's byte range and match by leading field name.
fn struct_field_docs(src: &str, start: usize, end: usize) -> Vec<(String, String)> {
    let slice = &src[start.min(src.len())..end.min(src.len())];
    let mut out = Vec::new();
    for line in slice.lines() {
        let Some((code, comment)) = line.split_once("///") else {
            continue;
        };
        // `name: type,` -> field name is the last ident before the first `:`
        // (last, so a `struct Foo {` prefix on the same line doesn't fool us).
        let Some((before_colon, _)) = code.split_once(':') else {
            continue;
        };
        let Some(fname) = before_colon.rsplit([' ', '\t', '{', ',', '(']).find(|s| !s.is_empty())
        else {
            continue;
        };
        if !is_ident(fname) {
            continue;
        }
        out.push((fname.to_string(), comment.trim().to_string()));
    }
    out
}

fn is_ident(s: &str) -> bool {
    let mut chars = s.chars();
    matches!(chars.next(), Some(c) if c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

// ---------------------------------------------------------------------------
// Line/byte bookkeeping for doc-comment recovery
// ---------------------------------------------------------------------------

/// Maps byte offsets to lines so doc comments (stripped before the AST) can be
/// read back out of the raw source.
struct LineIndex {
    /// (byte offset of line start, line text without the trailing newline).
    lines: Vec<(usize, String)>,
}

impl LineIndex {
    fn new(src: &str) -> Self {
        let mut lines = Vec::new();
        let mut offset = 0;
        for line in src.split_inclusive('\n') {
            let text = line.strip_suffix('\n').unwrap_or(line);
            let text = text.strip_suffix('\r').unwrap_or(text);
            lines.push((offset, text.to_string()));
            offset += line.len();
        }
        LineIndex { lines }
    }

    /// Index of the line containing `byte`.
    fn line_of(&self, byte: usize) -> usize {
        match self.lines.binary_search_by(|(start, _)| start.cmp(&byte)) {
            Ok(i) => i,
            // `Err(i)` is the first line starting after `byte`; the owner is the
            // one before it.
            Err(i) => i.saturating_sub(1),
        }
    }

    /// Contiguous `///` block directly above the item at `byte` (no blank gap),
    /// as rendered Markdown, or `None` if there isn't one.
    fn doc_above(&self, byte: usize) -> Option<String> {
        let item_line = self.line_of(byte);
        let mut collected: Vec<&str> = Vec::new();
        let mut i = item_line;
        while i > 0 {
            i -= 1;
            let text = self.lines[i].1.trim_start();
            if let Some(rest) = doc_body(text) {
                collected.push(rest);
            } else {
                break;
            }
        }
        if collected.is_empty() {
            return None;
        }
        collected.reverse();
        Some(collected.join("\n"))
    }

    /// A leading `///` block at the top of the file, but only when it's set off
    /// from the first item by a blank line (so it reads as module-level prose,
    /// not the first item's doc).
    fn module_doc(&self) -> Option<String> {
        let mut i = 0;
        let mut collected: Vec<&str> = Vec::new();
        while i < self.lines.len() {
            let text = self.lines[i].1.trim_start();
            match doc_body(text) {
                Some(rest) => {
                    collected.push(rest);
                    i += 1;
                }
                None => break,
            }
        }
        if collected.is_empty() {
            return None;
        }
        // Require a blank separator (or EOF) after the block; otherwise it's
        // glued to the first item and `doc_above` will claim it there.
        let separated = i >= self.lines.len() || self.lines[i].1.trim().is_empty();
        if separated {
            Some(collected.join("\n"))
        } else {
            None
        }
    }
}

/// If `line` (already left-trimmed) is a `///` doc comment, return its body with
/// the marker and one optional leading space stripped. `//` (non-doc) and code
/// return `None`.
fn doc_body(line: &str) -> Option<&str> {
    let rest = line.strip_prefix("///")?;
    // `////`-style dividers aren't doc text; treat as ordinary comments.
    if rest.starts_with('/') {
        return None;
    }
    Some(rest.strip_prefix(' ').unwrap_or(rest))
}

// ---------------------------------------------------------------------------
// Book scaffolding
// ---------------------------------------------------------------------------

fn write_summary(src_dir: &Path, pages: &[(String, PathBuf)]) -> Result<(), ()> {
    let mut summary = String::from("# Summary\n\n");
    for (title, path) in pages {
        summary.push_str(&format!("- [{}]({})\n", title, path.to_string_lossy().replace('\\', "/")));
    }
    let path = src_dir.join("SUMMARY.md");
    std::fs::write(&path, summary).map_err(|e| {
        eprintln!("havendoc: cannot write {}: {}", path.display(), e);
    })
}

fn write_book_toml(out: &Path, title: &str) -> Result<(), ()> {
    // The Oxocarbon theme reuses mdBook's built-in `navy` slot for its dark
    // variant (navy already selects the dark syntax-highlight stylesheet) and
    // `light` for its light variant; see `write_theme`. `navy` is therefore the
    // default and the preferred dark theme.
    let toml = format!(
        "[book]\ntitle = \"{}\"\nlanguage = \"en\"\nsrc = \"src\"\n\n\
         [output.html]\ndefault-theme = \"navy\"\npreferred-dark-theme = \"navy\"\n",
        title.replace('"', "\\\"")
    );
    let path = out.join("book.toml");
    std::fs::write(&path, toml).map_err(|e| {
        eprintln!("havendoc: cannot write {}: {}", path.display(), e);
    })
}

/// Oxocarbon (nyoom-engineering, an IBM Carbon Design System palette) as an
/// mdBook theme, emitted so a plain `havendoc` run needs no manual theming.
///
/// Only `theme/css/variables.css` is overridden: mdBook falls back to its
/// built-in `index.hbs`/`book.js` for everything else, so no mdBook internals
/// are vendored or version-pinned. The dark variant is mapped onto the built-in
/// `navy` theme slot (which already loads the dark highlight CSS) and the light
/// variant onto `light`; the trailing rules restrict the theme picker to just
/// those two and relabel them "Oxocarbon" / "Oxocarbon Light".
const THEME_VARIABLES_CSS: &str = include_str!("../assets/variables.css");

/// `theme/fonts/fonts.css`, overriding mdBook's default Open Sans / Source Code
/// Pro `@font-face` set with Geist / Geist Mono. Uses mdBook's `{{ resource }}`
/// helper, so it is rendered as a template at book-build time.
const THEME_FONTS_CSS: &str = include_str!("../assets/fonts.css");

/// `.gitignore` written into the generated output root, so the throwaway build
/// artifacts (mdBook's `book/` dir) aren't accidentally committed. Stored as
/// `.gitignore.txt` in the source tree so it isn't itself treated as a real
/// ignore file there.
const GITIGNORE: &str = include_str!("../assets/.gitignore.txt");

/// The Geist family (variable, weight axis 100-900) plus its SIL OFL license,
/// embedded so a plain `havendoc` run ships self-contained fonts. `(filename,
/// bytes)`; mdBook copies everything in `theme/fonts/` into the book and
/// content-hashes the woff2s. The `OFL.txt` accompanies the fonts as the license
/// requires.
const THEME_FONTS: &[(&str, &[u8])] = &[
    ("Geist.woff2", include_bytes!("../assets/fonts/Geist.woff2")),
    ("Geist-Italic.woff2", include_bytes!("../assets/fonts/Geist-Italic.woff2")),
    ("GeistMono.woff2", include_bytes!("../assets/fonts/GeistMono.woff2")),
    ("GeistMono-Italic.woff2", include_bytes!("../assets/fonts/GeistMono-Italic.woff2")),
    ("OFL.txt", include_bytes!("../assets/fonts/OFL.txt")),
];

/// Write the theme override under `<out>/theme`: the Oxocarbon color variables
/// (`css/variables.css`) plus the Geist fonts and their `@font-face`
/// declarations (`fonts/`). mdBook falls back to its built-in `index.hbs`/
/// `book.js`/`general.css` for everything else, so no mdBook internals are
/// vendored or version-pinned.
fn write_theme(out: &Path) -> Result<(), ()> {
    let theme = out.join("theme");

    let css_dir = theme.join("css");
    if let Err(e) = std::fs::create_dir_all(&css_dir) {
        eprintln!("havendoc: cannot create {}: {}", css_dir.display(), e);
        return Err(());
    }
    write_file(&css_dir.join("variables.css"), THEME_VARIABLES_CSS.as_bytes())?;

    let fonts_dir = theme.join("fonts");
    if let Err(e) = std::fs::create_dir_all(&fonts_dir) {
        eprintln!("havendoc: cannot create {}: {}", fonts_dir.display(), e);
        return Err(());
    }
    write_file(&fonts_dir.join("fonts.css"), THEME_FONTS_CSS.as_bytes())?;
    for (name, bytes) in THEME_FONTS {
        write_file(&fonts_dir.join(name), bytes)?;
    }
    Ok(())
}

/// Write `bytes` to `path`, reporting any error under the `havendoc:` prefix.
fn write_file(path: &Path, bytes: &[u8]) -> Result<(), ()> {
    std::fs::write(path, bytes).map_err(|e| {
        eprintln!("havendoc: cannot write {}: {}", path.display(), e);
    })
}
