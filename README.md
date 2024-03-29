# OrzCC

[![codecov](https://codecov.io/github/JuniMay/orzcc/graph/badge.svg?token=D7ZMIHWY5O)](https://codecov.io/github/JuniMay/orzcc)

Yet another compiler infrastructure.

## Getting Started

Just install rust toolchain and build/test/run using cargo.

## Testing

Install [cargo-tarpaulin](https://crates.io/crates/cargo-tarpaulin) to get test coverage.

```shell
cargo install cargo-tarpaulin
```

Note that because of the rustc version, you may need to install with `--locked` flag or nightly toolchain.

```shell
cargo install cargo-tarpaulin --locked
```

Then run the following command to get test coverage.

```shell
cargo tarpaulin
```

## Run Transformations

A simple example:

```shell
cargo run opt --file tests/orzir_cases/14_mem2reg.orzir --mem2reg
```
