use lalrpop_util::lalrpop_mod;
use regex::Regex;

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
            let replaced_line = _replace_starttime(line, lineno);
            result.push_str(&replaced_line);
        } else if re_stoptime.find(line).is_some() {
            let replaced_line = _replace_stoptime(line, lineno);
            result.push_str(&replaced_line);
        } else {
            result.push_str(line);
        }
        result.push('\n');
        lineno += 1;
    }
    result
}

fn _replace_starttime(line: &str, lineno: usize) -> String {
    let mut result = String::new();
    let re = Regex::new(r#"starttime\s*\(\s*\)"#).unwrap();
    if let Some(mat) = re.find(line) {
        let start = mat.start();
        let end = mat.end();
        result.push_str(&_copy_until_index(line, start));
        result.push_str(&_replace_lineno_start(lineno));
        result.push_str(&_copy_from_index(line, end));
    }
    result
}

fn _replace_stoptime(line: &str, lineno: usize) -> String {
    let mut result = String::new();
    let re = Regex::new(r#"stoptime\s*\(\s*\)"#).unwrap();
    if let Some(mat) = re.find(line) {
        let start = mat.start();
        let end = mat.end();
        result.push_str(&_copy_until_index(line, start));
        result.push_str(&_replace_lineno_stop(lineno));
        result.push_str(&_copy_from_index(line, end));
    }
    result
}

fn _replace_lineno_start(lineno: usize) -> String { format!("_sysy_starttime({})", lineno) }

fn _replace_lineno_stop(lineno: usize) -> String { format!("_sysy_stoptime({})", lineno) }

fn _copy_until_index(s: &str, index: usize) -> String { s[..index].to_string() }

fn _copy_from_index(s: &str, index: usize) -> String { s[index..].to_string() }

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
}
