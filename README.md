# OrzCC

[![codecov](https://codecov.io/github/JuniMay/orzcc/graph/badge.svg?token=D7ZMIHWY5O)](https://codecov.io/github/JuniMay/orzcc)

## 测试

### 单元测试 & 集成测试

可以使用 [cargo-tarpaulin](https://crates.io/crates/cargo-tarpaulin) 得到测试覆盖率

```shell
cargo install cargo-tarpaulin
```

之后使用如下命令运行

```shell
cargo tarpaulin
```

### SysY 样例测试

使用以下命令可以对 SysY 语言进行测试

```shell
python ./scripts/execute.py \
    --timeout 600 \
    --testcase-dir ./tests/sysy \
    --output-dir ./output --opt-level 1 \
    --runtime-lib-dir ./tests/sysy/sysy-runtime-lib
```

上述命令会按照 600 秒限时进行测试，从 `tests/sysy` 目录下获取所有 `.sy` 后缀的源代码编译、运
行并且对比输出。所有编译的结果、日志会存储在 `output` 目录下。运行前需要确保已经安装了
`riscv64-linux-gnu-xxx` 以及 `qemu-riscv64`，并且将 SysY 的运行时库放置于
`tests/sysy/sysy-runtime-lib` 目录下（也可以通过 `--runtime-lib-dir` 自行指定）。

## 文档

- [基础设施](./docs/infrastructure.md)
- [中间表示](./docs/ir.md)
- [优化/分析 Pass](./docs/passes.md)

## 调试

对于目标为 RISC-V 的情况，需要安装 `gdb-multiarch`。

```shell
sudo apt install gdb-multiarch
```

之后汇编

```shell
riscv64-linux-gnu-gcc -march=rv64gc ./test.s -L./tests/sysy/sysy-runtime-lib -lsylib -o ./test
```

然后使用 QEMU 运行，其中 `1234` 是端口

```shell
qemu-riscv64 -L /usr/riscv64-linux-gnu -g 1234 ./test < ./test.in &
```

之后可以使用 GDB 连接 QEMU

```shell
gdb-multiarch ./test
```

在 GDB 中输入

```shell
target remote :1234
```

之后调试即可。
