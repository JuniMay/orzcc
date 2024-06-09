pub mod ast;
pub mod irgen;
pub mod types;

pub use ast::*;
use lalrpop_util::lalrpop_mod;
use regex::Regex;
pub use types::*;

lalrpop_mod!(#[allow(clippy::all)] pub sysyparser, "/frontend/sysy/sysyparser.rs");

// replace starttime() with _sysy_starttime(lineno)
// replace stoptime() with _sysy_stoptime(lineno)
pub fn preprocess(context: &str) -> String {
    let re_starttime = Regex::new(r#"starttime\s*\(\s*\)"#).unwrap();
    let re_stoptime = Regex::new(r#"stoptime\s*\(\s*\)"#).unwrap();
    let mut result = String::new();
    let mut lineno = 1;
    for line in context.lines() {
        if re_starttime.find(line).is_some() {
            let replaced_line = replace_starttime(line, lineno);
            result.push_str(&replaced_line);
        } else if re_stoptime.find(line).is_some() {
            let replaced_line = replace_stoptime(line, lineno);
            result.push_str(&replaced_line);
        } else {
            result.push_str(line);
        }
        result.push('\n');
        lineno += 1;
    }
    result
}

fn replace_starttime(line: &str, lineno: usize) -> String {
    let mut result = String::new();
    let re = Regex::new(r#"starttime\s*\(\s*\)"#).unwrap();
    if let Some(mat) = re.find(line) {
        let start = mat.start();
        let end = mat.end();
        result.push_str(&copy_until_index(line, start));
        result.push_str(&replace_lineno_start(lineno));
        result.push_str(&copy_from_index(line, end));
    }
    result
}

fn replace_stoptime(line: &str, lineno: usize) -> String {
    let mut result = String::new();
    let re = Regex::new(r#"stoptime\s*\(\s*\)"#).unwrap();
    if let Some(mat) = re.find(line) {
        let start = mat.start();
        let end = mat.end();
        result.push_str(&copy_until_index(line, start));
        result.push_str(&replace_lineno_stop(lineno));
        result.push_str(&copy_from_index(line, end));
    }
    result
}

fn replace_lineno_start(lineno: usize) -> String { format!("_sysy_starttime({})", lineno) }

fn replace_lineno_stop(lineno: usize) -> String { format!("_sysy_stoptime({})", lineno) }

fn copy_until_index(s: &str, index: usize) -> String { s[..index].to_string() }

fn copy_from_index(s: &str, index: usize) -> String { s[index..].to_string() }

pub fn parse_hexadecimal_float(s: &str) -> f32 {
    let float_literal: hexponent::FloatLiteral = s.parse().unwrap();
    let result = float_literal.convert::<f64>(); // imprecise otherwise
    result.inner() as f32
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let context = r#"
        stoptime();
        starttime(); // This should also be replaced
        stoptime ( ); 
        
    "#;
        let processed = preprocess(context);
        println!("{}", processed);
    }

    #[test]
    fn test_hexadecimal_float() {
        let f = parse_hexadecimal_float("0x1.921fb6p+1");
        // 0x40490fdb, compare binary
        let f = f.to_bits();
        assert_eq!(f, 0x40490fdb);
    }
}
