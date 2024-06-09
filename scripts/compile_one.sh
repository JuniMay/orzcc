RUST_BACKTRACE=1 cargo run --bin compiler --release -- \
    -o test.s test.sy \
    --emit-ir test.orzir \
    --emit-typed-ast test.ast \
    --emit-pre-reg-alloc-asm test.asm -O1

riscv64-linux-gnu-gcc \
    -march=rv64gc ./test.s \
    -Lsysy-runtime-lib -lsylib -o ./test