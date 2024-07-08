# OrzCC

## 测试

可以使用 [cargo-tarpaulin](https://crates.io/crates/cargo-tarpaulin) 得到测试覆盖率

```shell
cargo install cargo-tarpaulin
```

之后使用如下命令运行

```shell
cargo tarpaulin
```

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
