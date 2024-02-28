use std::{
    fmt,
    io::{self, BufRead},
};

pub enum Level {
    Info,
    Warn,
    Error,
}

pub struct Diagnostic<'a> {
    source: &'a str,
    level: Level,
    filename: String,
    message: String,

    source_rows: (usize, usize),
    diagnostic_row: usize,
    diagnostic_cols: (usize, usize),
}

impl Diagnostic<'_> {
    pub fn new(
        source: &str,
        level: Level,
        filename: String,
        message: String,
        source_rows: (usize, usize),
        diagnostic_row: usize,
        diagnostic_cols: (usize, usize),
    ) -> Diagnostic<'_> {
        Diagnostic {
            source,
            level,
            filename,
            message,
            source_rows,
            diagnostic_row,
            diagnostic_cols,
        }
    }
}

impl<'a> fmt::Display for Diagnostic<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let level = match self.level {
            Level::Info => "info",
            Level::Warn => "warn",
            Level::Error => "error",
        };

        writeln!(f, "{}", level)?;

        let lineno_width = self.source_rows.1.to_string().len() - 1;

        let indent = " ".repeat(lineno_width);
        writeln!(
            f,
            "{} ---> {}:{}:{}",
            indent, self.filename, self.diagnostic_row, self.diagnostic_cols.0
        )?;

        // for each line within the span, the format is:
        // LL | <src>
        // if the line is within the diagnostic span, the format is:
        // LL | <src>
        //    | ^^^^^ <message>
        // seek to the start of the span
        let reader = io::BufReader::new(self.source.as_bytes());
        let lines = reader
            .lines()
            .skip(self.source_rows.0 - 1)
            .take(self.source_rows.1 - self.source_rows.0 + 1);

        let indent = " ".repeat(lineno_width + 1);
        writeln!(f, "{} |", indent)?;
        for line in lines {
            let line = line.unwrap();
            let lineno = self.source_rows.0;
            let lineno_str = lineno.to_string();
            let lineno_str = format!("{:width$}", lineno_str, width = lineno_width);

            writeln!(f, "{} | {}", lineno_str, line)?;

            if lineno == self.diagnostic_row {
                let col_width = self.diagnostic_cols.1 - self.diagnostic_cols.0;
                let carets = "^".repeat(col_width + 1);
                let space_before_carets = " ".repeat(self.diagnostic_cols.0 - 1);
                writeln!(
                    f,
                    "{} | {}{} {}",
                    indent, space_before_carets, carets, self.message
                )?;
            }
        }
        writeln!(f, "{} |", indent)?;
        Ok(())
    }
}
