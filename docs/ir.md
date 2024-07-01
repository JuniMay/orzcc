# 中间表示（IR）

中间表示的大致结构与 LLVM 类似，但是更加底层，使用基本块参数实现 SSA 的表示。

## Context

`Context` 中存储了整个 IR 的所有信息，包括指令、基本块、值、函数以及全局的符号等。`Context` 是一个 `Arena`，所有的数据都是通过 `Context` 进行分配和释放。

## 指令

指令是 IR 中定义 `Value` 的一种方式（另外一种方式是 Block Parameter，具体参考 `src/ir/value.rs`）。为了方便后续扩展，指令的结果设计成了多个，可以通过 `inst.result(ctx, idx)` 获得位于 `idx` 位置的结果（`idx` 从 0 开始）。

此处对 IR 中的指令进行简单的介绍，具体的实现以及指令的构建可以参考 `src/ir/inst.rs`。

### 常量定义指令

所有的常量都应当通过指令进行定义，常量的定义指令包括：

- `iconst`：定义整数常量，格式如：`%v1 = iconst 0xdeadbeef : i32`；
- `fconst`：定义浮点常量，格式如：`%v2 = fconst 0.2 : f32`；
- `undef`：未定义的值，格式如：`%v3 = undef : i32`。

### 栈上空间

栈上空间通过 `stack_slot` 声明，不需要指定类型，只需要指定大小即可。

```orzir
%v4 = stack_slot 4: ptr // 4 字节的栈上空间
```

### 二元运算

整数和浮点的运算分别是 `IBinary` 和 `FBinary`，目前暂时没有实现 SIMD 的支持，所以只支持标量运算。具体的操作码可以参考 `src/ir/inst.rs`。

### 一元运算以及类型转换

一元运算包括 `IUnary` 和 `FUnary`，目前只支持整数和浮点的取反操作。类型转换的操作码可以通过 `CastOp` 指定。

## 指针及地址

与 LLVM 不同，此处的 IR 不再使用 `getelementptr` 指令，而是通过 `offset` 进行计算（实际上与 `getelementptr` 一样，只是要求只有一个偏移量，偏移量的计算通过乘加实现）。

```orzir
%slot = stack_slot 4: ptr
%offset = iconst 3: i32
%addr = offset %slot, %offset : ptr
```

### 跳转指令

跳转指令包括 `br` 和 `jump`，其中 `br` 用于条件跳转，`jump` 用于无条件跳转。跳转时需要指定基本块以及传递的参数。跳转的目标块和传递的参数合并作为一个 `Successor` 类型。

### 调用指令

调用包括 `call` 和 `call_indirect`，`call` 指定函数的符号名称，`call_indirect` 指定函数的地址（函数指针）。通过 `call_indirect` 调用时需要指定函数的签名。

### 内存操作

由于全局的变量不是一个 `Value`，所以需要通过 `get_global` 指令获取对应的地址，之后通过 `load` 和 `store` 进行读写操作。
