# OrzCC

[![codecov](https://codecov.io/github/JuniMay/orzcc/graph/badge.svg?token=D7ZMIHWY5O)](https://codecov.io/github/JuniMay/orzcc)

## 架构

编译器使用 Rust 实现，SysY 前端使用 Lalrpop 生成，中间代码使用自定义设计完成的 IR，后端框架实
现时考虑了多后端的兼容问题，寄存器分配、基本块等主要组件能够复用，但是目前仍然只实现了 RISC-V 目
标架构的代码生成。

代码组织结构如下

```plain
├── src
│   ├── backend
│   │   ├── riscv64         // RISC-V 后端的实现
│   │   ├── ...             // 后端基础设施的实现
│   ├── bin
│   │   └── compiler.rs     // 编译器可执行文件
│   ├── collections
│   │   ├── ...             // 基础设施，具体内容可参考 `docs/infrastructure.md`
│   ├── frontend
│   │   ├── ir              // IR 文本的解析
│   │   ├── mod.rs
│   │   └── sysy            // SysY 前端的实现
│   ├── ir
│   │   ├── passes          // 优化/分析 Pass 的实现，具体内容可参考 `docs/passes.md`
│   │   ├── ...             // IR 数据结构的实现
│   ├── lib.rs
│   └── utils               // 通用的算法、数据结构等
```

## 文档

- [基础设施](./docs/infrastructure.md)
- [中间表示](./docs/ir.md)
- [优化/分析 Pass](./docs/passes.md)

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
qemu-riscv64 -cpu rv64,zba=true,zbb=true -L /usr/riscv64-linux-gnu -g 1234 ./test < ./test.in &
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

## Benchmarks

This directory contains a set of benchmarks that are used to evaluate the performance of the compiler. The benchmarks are transformed into the SysY language from the original C code. The benchmarks are divided into three categories as follows:

- [PolyBench/C 4.2](https://master.dl.sourceforge.net/project/polybench/polybench-c-4.2.tar.gz?viasf=1) -> **23** cases
- [Livermore loops](https://www.netlib.org/benchmark/livermorec) -> **6** cases
- [Mälardalen WCET Benchmarks](http://www.mrtc.mdh.se/projects/wcet/benchmarks.html) -> **2** cases
