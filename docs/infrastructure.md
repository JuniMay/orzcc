# 基础设施

基础设施主要是为了实现代码的复用，并且便于单元测试。基础设施有关的代码主要位于以下两个文件夹

- `src/collections`：主要包括一些数据结构的实现
  - `apint.rs`：运行时定长高精度整数，用于支持 IR 中的整数类型
  - `diagnostic.rs`：诊断信息，用于在解析 IR 源代码时报告错误
  - `linked_list.rs`：基于 Arena 的侵入式链表，包括一系列 Traits
  - `storage.rs`：用于实现 IR 中的 Arena 存储
- `src/utils`：主要是用于分析程序结构的基础算法
  - `cfg.rs`：对控制流图的抽象，包括 `CfgNode` 和 `CfgRegion`
  - `def_use.rs`：用于实现 Def-Use Chain 的 Traits
  - `dfs.rs`：参考 Cranelift 实现的在 CFG 上的深度优先遍历
  - `dominance.rs`：支配树、支配边界的计算

## Arena 存储

`src/collections/storage.rs` 中实现了 Arena 数据结构。Arena 是为了更方便地实现链表。主要的 Trait 包括

- `ArenaPtr`：Arena 中的指针，用于访问 Arena 中的数据
- `ArenaDeref`：表示一个可以对 ArenaPtr 解引用的容器
- `ArenaAlloc`：表示一个可以在 Arena 中分配数据的容器
- `ArenaFree`：表示一个可以在 Arena 中释放数据的容器

此外，为了更加方便使用，可以将多个 Arena 进行组合，通过指针的类型确定从哪一个容器中取数据。具体可以参考 `storage` 的文档

## 链表

链表主要用于 IR 中 `Block` 和 `Inst` 的表示。`src/collections/linked_list.rs` 中包括两个主要的 Trait：

- `LinkedListContainerPtr`：链表容器，同时也是一个 `ArenaPtr`。可以通过 `head` `tail` 获取链表的头尾节点。例如，对于指令，基本块就是其容器，`head` 获取的就是入口指令，对于基本块，函数就是其容器，`head` 获取的就是入口基本块。
- `LinkedListNodePtr`：链表节点，同时也是一个 `ArenaPtr`。可以通过 `prev` `next` 获取前后节点。

此外，为了方便遍历和编辑，提供了两种遍历方式：

- 不可变：可以直接使用 `.iter()` 从容器中获取节点的迭代器，但是在遍历的时候不可编辑，因为迭代器会 borrow Arena（在 IR 中就是借用 `Context`）；
- 可变：可以使用 `.cursor()` 获取一个 `LinkedListCursor`，可以在遍历的时候编辑链表，但是需要手动迭代
  
注意，`cursor.next()` 总是会返回当前访问节点在**调用时**的下一个节点，也就是说，如果对某个节点的下一个节点进行了删除操作，那么 `cursor.next()` 会跳过这个节点。如果将某个节点从链表中断开，那么迭代就会结束。此外，如果调用 `prev()` 会从整个链表的尾部开始迭代，相当于普通迭代器调用 `rev()` 之后再调用 `next()`（或者 `next_back`）。

对于节点的断开操作，应当尽可能避免在遍历的时候进行，而应当先进行记录，然后在遍历结束后进行删除。

## 控制流图

`src/utils/cfg.rs` 中实现了对控制流图的抽象。主要包括两个 Trait：

- `CfgNode`：表示控制流图中的节点，即基本块；
- `CfgRegion`：表示控制流图中的区域，即函数。

可以通过 `CfgRegion::entry_node` 获取入口节点，对于 IR 来说，入口节点总是函数中基本块链表的头节点。

另外，还提供了 `CfgInfo` 用于获取某个 `CfgNode` 的前驱。具体使用可以参考 `cfg` 的文档。

## Def-Use Chain

`src/utils/def_use.rs` 中实现了 Def-Use Chain 的 Trait。主要包括两个 Trait：

- `Usable`：表示某个对象可以被使用，且可以通过 `users` 函数获得当前的所有使用者；
- `User<T>`：表示某个对象可以使用类型为 `T` 的对象（`T` 必须实现 `Usable` Trait），可以通过 `replace` 替换使用的对象。

另外，还提供了 `Operand` 这个类型。`Operand` 不可 `Clone`，在使用 `new` 创建时必须指定使用者。但是需要注意的是，`Operand` 中的方法都比较底层，一般情况下不需要直接使用，而可以指令中封装的方法。

## DFS

DFS 是参考 Cranelift 中的实现完成的，可以以迭代器的方式在控制流图上进行深度优先遍历。主要的结构包括：

- `Event`：表示 DFS 中的事件，包括 `Enter` 和 `Leave`；
- `DfsContext`：表示 DFS 的上下文，包括使用的栈以及 `visited` 集合；

另外还有 `DfsIterator`，`DfsPreOrderIterator` 和 `DfsPostOrderIterator` 三种迭代器。`DfsIterator` 会返回访问的节点以及事件，另外两个节点会返回前序和后序遍历下的节点。

按照目前的设计，每个节点在同一个事件下应当会被且仅会被访问一次（对于 `DfsIterator` 来说就是两次，除非这个节点不可达）。

## 支配信息

Dominance 的计算采用了 *A Simple, Fast Dominance Algorithm* 中 intersect 的方法。具体使用可以参考 `dominance` 的文档。
