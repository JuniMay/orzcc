use std::env;

fn main() {
    let frontend_sysy_enabled = env::var("CARGO_FEATURE_FRONTEND_SYSY").is_ok();

    if frontend_sysy_enabled {
        lalrpop::process_root().unwrap();
    }
}
