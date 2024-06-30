use hexponent::FloatLiteral;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(clippy::all)] pub parser, "/frontend/sysy/parser.rs");

/// Parse a given hexadecimal float string into a f32.
pub fn parse_hexadecimal_float(s: &str) -> f32 {
    let float_literal: FloatLiteral = s.parse().unwrap();
    // because we want it to be precise, so we have to use a temp f64.
    let result = float_literal.convert::<f64>();
    result.inner() as f32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hexadecimal_float() {
        let f = parse_hexadecimal_float("0x1.921fb6p+1");
        // 0x40490fdb, compare binary
        let f = f.to_bits();
        assert_eq!(f, 0x40490fdb);
    }
}
