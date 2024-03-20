use lalrpop_util::lalrpop_mod;

// The sysy.rs file will contain the generated parser which laies in the target/debug/build/out/frontend/sysy/sysy.rs.

lalrpop_mod!(pub sysyparser, "/frontend/sysy/sysyparser.rs");

#[cfg(test)]
mod tests {
    //use crate::frontend::sysy::sysyparser;
    #[test]
    fn test_sysy() {
        // valid-cases
        let valid_cases = vec![
            "int main() { return 0; }",
            "int main() { int a; return 0; }",
            // add more valid cases here
        ];

        for case in valid_cases {
            //assert!(sysyparser::ProgramParser::new().parse(&case).is_ok(), "Failed to parse valid case: {}", case);
        }

        // invalid-cases
        let invalid_cases = vec![
            "int main() { return; }",
            // add more invalid cases here
        ];

        for case in invalid_cases {
            //assert!(sysyparser::ProgramParser::new().parse(&case).is_err(), "Incorrectly parsed invalid case: {}", case);
        }
    }
}
