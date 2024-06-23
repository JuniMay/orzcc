//! # Diagnostic
//!
//! This module is a very simple diagnostic utility inspired by
//! [ariadne](https://github.com/zesterer/ariadne).

use std::{
    collections::{HashMap, HashSet},
    fmt,
    ops::Range,
};

/// The context to store all diagnostic snippets.
#[derive(Debug, Default)]
pub struct DiagnosticContext {
    /// The snippets in the context.
    snippets: Vec<DiagnosticSnippet>,
}

impl DiagnosticContext {
    pub fn snippets(&self) -> &[DiagnosticSnippet] { &self.snippets }
}

#[derive(Debug)]
pub enum Severity {
    Warn,
    Error,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Severity::Warn => write!(f, "Warning"),
            Severity::Error => write!(f, "Error"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Annotation {
    /// The span of this diagnostic annotation.
    span: Range<usize>,
    /// The message of this sub-diagnostic annotation.
    message: String,
}

impl Annotation {
    pub fn new(span: Range<usize>, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

/// The character set to display.
pub struct CharSet {
    /// The horizontal bar character.
    pub hbar: char,
    /// The vertical bar character.
    pub vbar: char,
    /// The dotted vertical bar character.
    ///
    /// This is used when some lines are skipped.
    pub vbar_dotted: char,
    /// The arrow character.
    pub arrow: char,
    /// The left top corner character.
    pub ltop: char,
    /// The left bottom corner character.
    pub lbottom: char,
    /// The left cross character.
    pub lcross: char,
    /// The underline character.
    pub underline: char,
    /// The underline character with a vertical bar as indicator.
    pub underline_vbar: char,
}

impl CharSet {
    /// Get the ASCII character set.
    pub fn ascii() -> Self { ASCII }

    /// Get the Unicode character set.
    pub fn unicode() -> Self { UNICODE }

    /// Get the Unicode character set with rounded corners.
    pub fn unicode_round() -> Self { UNICODE_ROUND }
}

const ASCII: CharSet = CharSet {
    hbar: '-',
    vbar: '|',
    vbar_dotted: ':',
    arrow: '>',
    ltop: ',',
    lbottom: '`',
    lcross: '|',
    underline: '~',
    underline_vbar: '~',
};

const UNICODE: CharSet = CharSet {
    hbar: '─',
    vbar: '│',
    vbar_dotted: '┆',
    arrow: '▶',
    ltop: '┌',
    lbottom: '└',
    lcross: '├',
    underline: '─',
    underline_vbar: '┬',
};

const UNICODE_ROUND: CharSet = CharSet {
    hbar: '─',
    vbar: '│',
    vbar_dotted: '┆',
    arrow: '▶',
    ltop: '╭',
    lbottom: '╰',
    lcross: '├',
    underline: '─',
    underline_vbar: '┬',
};

pub struct RenderOptions {
    /// The character set to use.
    charset: CharSet,
    /// The left margin size by character.
    margin: usize,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            charset: ASCII,
            margin: 2,
        }
    }
}

impl RenderOptions {
    pub fn new(charset: CharSet, margin: usize) -> Self { Self { charset, margin } }
}

/// Adiagnostic snippet.
#[derive(Debug)]
pub struct DiagnosticSnippet {
    /// The severity of the diagnostic.
    severity: Severity,
    /// The optional code of the diagnostic.
    code: Option<String>,
    /// The primary message of this snippet.
    message: String,
    /// The annotationrmation related to this snippet.
    annotations: Vec<Annotation>,
    /// The additional notes.
    notes: Vec<(String, String)>,
}

impl DiagnosticSnippet {
    pub fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            code: None,
            message: message.into(),
            annotations: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Set the code of this diagnostic.
    pub fn code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Add an annotation to this diagnostic.
    pub fn annotate(mut self, span: Range<usize>, message: impl Into<String>) -> Self {
        self.annotations.push(Annotation::new(span, message));
        self
    }

    /// Add a note to this diagnostic.
    pub fn note(mut self, name: impl Into<String>, note: impl Into<String>) -> Self {
        self.notes.push((name.into(), note.into()));
        self
    }

    /// Render this diagnostic snippet.
    fn render<'a>(self, src: &'a str, options: &'a RenderOptions) -> DiagnosticRenderer<'a> {
        DiagnosticRenderer {
            src,
            options,
            snippet: self,
        }
    }
}

pub struct DiagnosticRenderer<'a> {
    src: &'a str,
    options: &'a RenderOptions,
    snippet: DiagnosticSnippet,
}

impl fmt::Display for DiagnosticRenderer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let charset = &self.options.charset;

        // the set of lines that have annotations
        let mut annotated_lines = HashSet::new();
        // the underline ranges (related to the start of the line) for each lines
        let mut underlines: HashMap<usize, Vec<(usize, usize)>> = HashMap::new();
        // the inline annotations (not covering multiple lines)
        let mut inline_annotations: HashMap<usize, Vec<(usize, &Annotation)>> = HashMap::new();
        // the block annotations (covering multiple lines)
        let mut block_annotations = Vec::new();

        // the maximum line number
        let mut max_lineno = 0;
        // the start offset in the source
        let mut start_offset = 0;
        // the end offset in the source
        let mut end_offset = 0;

        for annotation in self.snippet.annotations.iter() {
            // the start line number of this annotation
            let start_lineno = self.src[..annotation.span.start].lines().count().max(1);
            // the end line number of this annotation
            let end_lineno = self.src[..annotation.span.end].lines().count();

            if start_lineno == end_lineno {
                // this is an inline annotation

                // jump to the start of the line
                let line_start_offset = self.src[..annotation.span.start]
                    .rfind('\n')
                    .map_or(0, |idx| idx + 1);

                underlines.entry(start_lineno).or_default().push((
                    annotation.span.start - line_start_offset,
                    annotation.span.end - line_start_offset,
                ));
                // the annotation position is in the middle of the underline/span
                let annotation_pos =
                    (annotation.span.start + annotation.span.end) / 2 - line_start_offset;
                inline_annotations
                    .entry(start_lineno)
                    .or_default()
                    .push((annotation_pos, annotation));
            } else {
                // this is a block annotation
                block_annotations.push((start_lineno, end_lineno, annotation));
            }

            annotated_lines.insert(start_lineno);
            annotated_lines.insert(end_lineno);

            max_lineno = max_lineno.max(end_lineno);
            start_offset = start_offset.min(annotation.span.start);
            end_offset = end_offset.max(annotation.span.end);
        }

        let start_offset = self.src[..start_offset]
            .rfind('\n')
            .map_or(0, |idx| idx + 1);
        let end_offset = self.src[end_offset..]
            .find('\n')
            .map_or(self.src.len(), |idx| end_offset + idx);

        let lineno_width = max_lineno.to_string().len() + self.options.margin;

        block_annotations.sort_by_key(|(_, _, annotation)| annotation.span.start);

        for (_lineno, annotations) in inline_annotations.iter_mut() {
            annotations.sort_by_key(|(pos, _)| *pos);
        }

        // merge the underlines
        for (_lineno, underlines) in underlines.iter_mut() {
            let mut merged = Vec::new();
            let mut prev = underlines[0];
            for underline in underlines.iter().skip(1) {
                if prev.1 >= underline.0 {
                    prev.1 = underline.1;
                } else {
                    merged.push(prev);
                    prev = *underline;
                }
            }
            merged.push(prev);
            underlines.clear();
            underlines.extend(merged);
        }

        let headline = match self.snippet.code {
            Some(ref code) => format!(
                "[{}] {}: {}",
                code, self.snippet.severity, self.snippet.message
            ),
            None => format!("[{}] {}", self.snippet.severity, self.snippet.message),
        };

        writeln!(
            f,
            "{}{}{}{} {}",
            " ".repeat(lineno_width),
            charset.hbar,
            charset.hbar,
            charset.arrow,
            headline
        )?;

        let mut skipped = false;

        for (lineno, line) in self.src[start_offset..end_offset].lines().enumerate() {
            let lineno = lineno + 1;
            if !annotated_lines.contains(&lineno) {
                if !skipped {
                    // this is the first line to skip, draw the dotted lines
                    write!(f, "{} {}", " ".repeat(lineno_width), charset.vbar_dotted)?;

                    for (start_lineno, end_lineno, _) in block_annotations.iter() {
                        if lineno != *start_lineno && lineno != *end_lineno {
                            write!(f, " {}", charset.vbar_dotted)?;
                        } else {
                            unreachable!("start and end lineno should in the annotated lines");
                        }
                    }

                    writeln!(f)?;
                    skipped = true;
                }
                continue;
            }
            skipped = false;

            let leading = format!("{:width$} {} ", lineno, charset.vbar, width = lineno_width);
            write!(f, "{}", leading)?;

            let mut has_started = false;
            let mut has_ended = false;
            // the left frame for block annotations of the next line.
            let mut frames_below = String::new();
            for (start_lineno, end_lineno, _) in block_annotations.iter() {
                if lineno == *start_lineno {
                    write!(f, "{}{}", charset.ltop, charset.hbar)?;
                    has_started = true;
                    frames_below.push(charset.vbar);
                    frames_below.push(' ');
                } else if lineno == *end_lineno {
                    write!(f, "{}{}", charset.lcross, charset.hbar)?;
                    has_ended = true;
                    frames_below.push(charset.vbar);
                    frames_below.push(' ');
                } else if lineno > *start_lineno {
                    if has_ended {
                        write!(f, "{}{}", charset.hbar, charset.hbar)?;
                    } else {
                        write!(f, "{} ", charset.vbar)?;
                    }
                    frames_below.push(charset.vbar);
                    frames_below.push(' ');
                } else if has_started {
                    write!(f, "{}{}", charset.hbar, charset.hbar)?;
                    frames_below.push_str(" ".repeat(2).as_str());
                } else {
                    write!(f, "{:width$}", "", width = 2)?;
                    frames_below.push_str(" ".repeat(2).as_str());
                }
            }

            if has_started || has_ended {
                // an arrow of the indicators
                write!(f, "{}{} ", charset.hbar, charset.arrow)?;
            } else {
                // just spaces
                write!(f, "{:width$}", "", width = 3)?;
            }
            frames_below.push_str(" ".repeat(3).as_str());

            writeln!(f, "{}", line)?;

            if !underlines.contains_key(&lineno) {
                continue;
            }

            let leading = format!(
                "{:width$} {} {}",
                "",
                charset.vbar,
                frames_below,
                width = lineno_width
            );
            let annotations = inline_annotations.get(&lineno).unwrap();

            let mut annotation_pos = HashSet::new();
            for (pos, _) in annotations.iter() {
                annotation_pos.insert(*pos);
            }

            write!(f, "{}", leading)?;
            // draw the underline
            let mut prev_end = 0;
            for (start, end) in underlines.get(&lineno).unwrap() {
                write!(f, "{}", " ".repeat(*start - prev_end))?;

                for idx in *start..=*end {
                    if annotation_pos.contains(&idx) {
                        write!(f, "{}", charset.underline_vbar)?;
                    } else {
                        write!(f, "{}", charset.underline)?;
                    }
                }

                prev_end = *end + 1;
            }

            writeln!(f)?;

            for (pos, annotation) in annotations.iter() {
                write!(f, "{}", leading)?;
                let mut prev_pos = 0;
                for (curr_pos, _) in annotations.iter() {
                    if *curr_pos < *pos {
                        continue;
                    }
                    if *curr_pos < prev_pos {
                        continue;
                    }
                    write!(f, "{}", " ".repeat(curr_pos - prev_pos))?;
                    write!(f, "{}", charset.vbar)?;
                    prev_pos = *curr_pos + 1;
                }

                writeln!(f)?;
                write!(f, "{}{}{}", leading, " ".repeat(*pos), charset.lbottom)?;

                let end = if line.len() > *pos {
                    line.len() - *pos - 1
                } else {
                    0
                };

                for _ in 0..end {
                    write!(f, "{}", charset.hbar)?;
                }
                writeln!(f, "{} {}", charset.hbar, annotation.message)?;
            }
        }

        for idx in (0..block_annotations.len()).rev() {
            write!(f, "{} {} ", " ".repeat(lineno_width), charset.vbar)?;
            for _ in 0..=idx {
                write!(f, "{} ", charset.vbar)?;
            }
            writeln!(f)?;
            write!(f, "{} {} ", " ".repeat(lineno_width), charset.vbar)?;
            for _ in 0..idx {
                write!(f, "{} ", charset.vbar)?;
            }
            write!(f, "{}", charset.lbottom)?;
            for _ in 0..(block_annotations.len() - 1 - idx) {
                write!(f, "{}", charset.hbar.to_string().repeat(2))?;
            }
            writeln!(
                f,
                "{} {}",
                charset.hbar.to_string().repeat(3),
                block_annotations[idx].2.message
            )?;
        }
        writeln!(f, "{} {} ", " ".repeat(lineno_width), charset.vbar)?;
        for (name, note) in self.snippet.notes.iter() {
            writeln!(f, "{} = {}: {}", " ".repeat(lineno_width), name, note)?;
        }

        Ok(())
    }
}

impl DiagnosticContext {
    pub fn new() -> Self { Self::default() }

    /// Render all the snippets in the context.
    pub fn render(&mut self, src: &str, options: &RenderOptions) -> String {
        let mut result = String::new();
        for snippet in self.snippets.drain(..) {
            let s = format!("{}", snippet.render(src, options));
            result.push_str(s.as_str());
        }
        result
    }

    /// Add a snippets
    pub fn push(&mut self, snippet: DiagnosticSnippet) { self.snippets.push(snippet); }

    /// Merge another diagnostic context into this one.
    pub fn merge(&mut self, other: &mut DiagnosticContext) {
        self.snippets.append(&mut other.snippets);
    }

    /// Merge another diagnostic context with a filter.
    pub fn merge_if<F>(&mut self, other: DiagnosticContext, f: F)
    where
        F: Fn(&DiagnosticSnippet) -> bool,
    {
        self.snippets
            .extend(other.snippets.into_iter().filter(|snippet| f(snippet)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_context() {
        let src =
            "fn main() {\n    println!(\"Hello, world!\");\n}\n\n\n\n// just a random comment\n";
        let mut ctx = DiagnosticContext::new();

        let snippet = DiagnosticSnippet::new(Severity::Error, "expected `;`")
            .code("E001")
            .annotate(16..17, "expected `;`")
            .annotate(17..18, "expected `;`")
            .annotate(25..39, "THIS IS A STRING LITERAL")
            .note("note", "try adding a semicolon")
            .annotate(4..44, "expected `;`")
            .annotate(4..40, "expected `;`")
            .annotate(20..53, "expected `;`");

        ctx.push(snippet);

        let rendered = ctx.render(
            src,
            &RenderOptions {
                charset: ASCII,
                margin: 2,
            },
        );

        // trim the rendered string to remove the trailing whitespace of each line
        let rendered = rendered
            .lines()
            .map(|l| l.trim_end())
            .collect::<Vec<_>>()
            .join("\n");

        let expected = r#"   --> [E001] Error: expected `;`
  1 | ,-,----> fn main() {
  2 | | |-,-->     println!("Hello, world!");
    | | | |        ~~~      ~~~~~~~~~~~~~~~
    | | | |        ||              |
    | | | |        `-------------------------- expected `;`
    | | | |         |              |
    | | | |         `------------------------- expected `;`
    | | | |                        |
    | | | |                        `---------- THIS IS A STRING LITERAL
  3 | |------> }
    : : : :
  7 | | | |--> // just a random comment
    | | | |
    | | | `--- expected `;`
    | | |
    | | `----- expected `;`
    | |
    | `------- expected `;`
    |
    = note: try adding a semicolon"#;
        assert_eq!(rendered, expected);
        println!("{}", rendered);
    }

    #[test]
    fn test_0() {
        let src = "fn main() {";
        let mut ctx = DiagnosticContext::new();

        let snippet = DiagnosticSnippet::new(Severity::Error, "unexpected end of input")
            .code("E001")
            .annotate(0..1, "in the function")
            .annotate(0..1, "in the function")
            .annotate(0..1, "in the function")
            .annotate(0..1, "in the function")
            .annotate(10..10, "expected `}`")
            .annotate(10..10, "expected `}`")
            .annotate(10..10, "expected `}`")
            .note("help", "try adding a closing brace".to_string());

        ctx.push(snippet);

        let rendered = ctx.render(
            src,
            &RenderOptions {
                charset: ASCII,
                margin: 2,
            },
        );
        let rendered = rendered
            .lines()
            .map(|l| l.trim_end())
            .collect::<Vec<_>>()
            .join("\n");

        let expected = r#"   --> [E001] Error: unexpected end of input
  1 |    fn main() {
    |    ~~        ~
    |    |         |
    |    `----------- in the function
    |    |         |
    |    `----------- in the function
    |    |         |
    |    `----------- in the function
    |    |         |
    |    `----------- in the function
    |              |
    |              `- expected `}`
    |              |
    |              `- expected `}`
    |              |
    |              `- expected `}`
    |
    = help: try adding a closing brace"#;

        assert_eq!(rendered, expected);
        println!("{}", rendered);
    }
}
