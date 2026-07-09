use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind};

use crate::front::ast::{Error, Span};

/// `(file-key, source-text)` for every module a diagnostic might point into.
/// Built once by the module loader (keys match `Span::file`) and threaded down
/// so any stage can quote the owning module's source, not the entry file's.
pub type Sources<'a> = Vec<(String, &'a str)>;

/// AST/chumsky spans are byte offsets, but ariadne's renderer indexes labels by
/// char offset. Convert here so multibyte source still underlines the right span
fn byte_to_char(src: &str, byte: usize) -> usize {
    src[..byte.min(src.len())].chars().count()
}

fn to_span(src: &str, span: &Span) -> (String, Range<usize>) {
    let start = byte_to_char(src, span.start);
    let end = byte_to_char(src, span.end.max(span.start));
    (span.file.clone(), start..end)
}

fn source_of<'a>(sources: &Sources<'a>, file: &str) -> Option<&'a str> {
    sources.iter()
        .find(|(k, _)| k == file)
        .map(|(_, s)| *s)
}

/// Render one diagnostic to stderr. `stage` is the header label (e.g.
/// `"Typecheck error"`); `span.file` selects which source in `sources` to quote.
pub fn report(stage: &str, msg: &str, span: &Span, sources: &Sources) {
    let Some(src) = source_of(sources, &span.file) else {
        // no source on hand (shouldn't happen) but we don't want to swallow the
        // message
        eprintln!("{} in {}: {}", stage, span.file, msg);
        return;
    };

    let span = to_span(src, span);
    Report::build(ReportKind::Custom(stage, Color::Red), span.clone())
        .with_message(msg)
        .with_label(Label::new(span).with_color(Color::Red).with_message(msg))
        .finish()
        // ariadne indexes each Source by char offset - matches `to_span` above
        .eprint(ariadne::sources(sources.iter().map(|(k, s)| (k.clone(), *s))))
        .ok();
}

/// Convenience for the common `ast::Error` case.
pub fn report_error(stage: &str, err: &Error, sources: &Sources) {
    report(stage, &err.msg, &err.span, sources);
}
