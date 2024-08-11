# This is for recording the scripts

## test.oracle.py

```shell
python3 ./scripts/test_oracle.py ./tests/sysy/external_test/00.sy --timeout 600 --opt-level 1 --output-dir ./output --executable-path ./target/release/compiler --no-compile
```

## execute.py

```shell
python3 ./scripts/execute.py     --timeout 600     --testcase-dir ./tests/sysy/external_test     --output-dir ./output --opt-level 1     --runtime-lib-dir ./tests/sysy/sysy-runtime-lib
```