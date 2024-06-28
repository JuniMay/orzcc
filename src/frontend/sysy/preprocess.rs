//! Preprocess of SysY source file.
//!
//! The only macro replacement in SysY is `starttime` and `stoptime`.
//! - `starttime()` is replaced by `_sysy_starttime(__LINE__)`
//! - `stoptime()` is replaced by `_sysy_stoptime(__LINE__)`
//!
//! Additionally, the `__LINE__` is replaced by the line number of the source
//! file.
//!
//! Note that the comments are not removed to preserve the line number. Hope
//! that there will be no block comments inside the `starttime` and `stoptime`
//! macro.

use regex::Regex;

fn replace_starttime(line: &str, lineno: usize) -> String {
    let mut result = String::new();
    let re = Regex::new(r"starttime\s*\(\s*\)").unwrap();
    if let Some(caps) = re.captures(line) {
        // replace all occurrences
        let mut last = 0;
        for cap in caps.iter().flatten() {
            result.push_str(&line[last..cap.start()]);
            result.push_str(&format!("_sysy_starttime({})", lineno));
            last = cap.end();
        }
        result.push_str(&line[last..]);
    } else {
        result.push_str(line);
    }
    result
}

fn replace_stoptime(line: &str, lineno: usize) -> String {
    let mut result = String::new();
    let re = Regex::new(r"stoptime\s*\(\s*\)").unwrap();
    if let Some(caps) = re.captures(line) {
        // replace all occurrences
        let mut last = 0;
        for cap in caps.iter().flatten() {
            result.push_str(&line[last..cap.start()]);
            result.push_str(&format!("_sysy_stoptime({})", lineno));
            last = cap.end();
        }
        result.push_str(&line[last..]);
    } else {
        result.push_str(line);
    }
    result
}

/// Preprocess the source code of SysY.
///
/// The preprocessing is relatively simple. The only thing to do is to replace
/// `starttime()` and `stoptime()`.
pub fn preprocess(src: &str) -> String {
    let mut result = String::new();
    let mut lineno = 1;
    for line in src.lines() {
        let line = replace_starttime(line, lineno);
        let line = replace_stoptime(&line, lineno);
        result.push_str(&line);
        result.push('\n');
        lineno += 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preprocess() {
        let src = r#"
{
        starttime();
        stoptime  (  ) ;
        starttime ();
        stoptime( ); int main() {}
}
"#;
        let expected = r#"
{
        _sysy_starttime(3);
        _sysy_stoptime(4) ;
        _sysy_starttime(5);
        _sysy_stoptime(6); int main() {}
}
"#;

        assert_eq!(preprocess(src), expected);
    }
}
