///to be modify

use crate::ir::{
    module::Module,
    values::{Block, Inst},
    layout::{BlockNode, Layout, BlockList},
};

use std::collections::{HashMap, HashSet};
use id_arena::Id;

pub type BasicBlockId = Id<BasicBlock>;

pub struct BasicBlock {
    pub pred: HashSet<BasicBlockId>,
    pub succ: HashSet<BasicBlockId>,
}

impl BasicBlock {
    // 检查是否有后继基本块
    pub fn succ_empty(&self) -> bool {
        self.succ.is_empty()
    }

    // 检查是否有前驱基本块
    pub fn pred_empty(&self) -> bool {
        self.pred.is_empty()
    }

    // 添加后继基本块
    pub fn add_succ(&mut self, block_id: BasicBlockId) {
        self.succ.insert(block_id);
    }

    // 移除后继基本块
    pub fn remove_succ(&mut self, block_id: &BasicBlockId) {
        self.succ.remove(block_id);
    }

    // 添加前驱基本块
    pub fn add_pred(&mut self, block_id: BasicBlockId) {
        self.pred.insert(block_id);
    }

    // 移除前驱基本块
    pub fn remove_pred(&mut self, block_id: &BasicBlockId) {
        self.pred.remove(block_id);
    }

    // 获取后继基本块的迭代器
    pub fn succ_iter(&self) -> impl Iterator<Item = &BasicBlockId> {
        self.succ.iter()
    }

    // 获取前驱基本块的迭代器
    pub fn pred_iter(&self) -> impl Iterator<Item = &BasicBlockId> {
        self.pred.iter()
    }

    // 获取前驱基本块的数量
    pub fn get_num_of_pred(&self) -> usize {
        self.pred.len()
    }

    // 获取后继基本块的数量
    pub fn get_num_of_succ(&self) -> usize {
        self.succ.len()
    }
}

    //reference
    // //根据基本块的前驱、后继关系进行流图的构造
    // //遍历函数的所有基本块，为每个基本块处理控制流逻辑
    // for (auto block = func->begin(); block != func->end(); block++)
    // {
    //     //获取BB的第一条和最后一条指令
    //     Instruction *i = (*block)->begin();
    //     Instruction *last = (*block)->rbegin();
    //     //再次循环遍历指令
    //     while (i != last)
    //     {
    //         if (i->isCond() || i->isUncond())
    //         {
    //             //移除块中跳转指令
    //             (*block)->remove(i);
    //         }
    //         i = i->getNext();
    //     }
    //     //判断最后一条指令的类型并进行相应处理：
    //     //如果是条件跳转，处理真假分支并建立基本块间的前驱后继关系。
    //     if (last->isCond())
    //     {
    //         BasicBlock *truebranch, *falsebranch;
    //         truebranch =
    //             dynamic_cast<CondBrInstruction *>(last)->getTrueBranch();
    //         falsebranch =
    //             dynamic_cast<CondBrInstruction *>(last)->getFalseBranch();
    //         (*block)->addSucc(truebranch);
    //         (*block)->addSucc(falsebranch);
    //         truebranch->addPred(*block);
    //         falsebranch->addPred(*block);
    //     }
    //     //如果是无条件跳转，处理跳转目标并建立基本块间的链接
    //     else if (last->isUncond())
    //     {
    //         //获取要跳转的基本块
    //         BasicBlock *dst =
    //             dynamic_cast<UncondBrInstruction *>(last)->getBranch();
    //         //跳转块链接
    //         (*block)->addSucc(dst);
    //         dst->addPred(*block);
    //     }
    //     //如果不是返回指令
    //     else if (!last->isRet())
    //     {
    //         if (((FunctionType *)(se->getType()))->getRetType() == TypeSystem::voidType)
    //         {
    //             new RetInstruction(nullptr, *block);
    //         }
    //         else if (((FunctionType *)(se->getType()))->getRetType() == TypeSystem::floatType)
    //         {
    //             new RetInstruction(new Operand(new ConstantSymbolEntry(TypeSystem::floatType, 0)), *block);
    //         }      
    //         else if (((FunctionType *)(se->getType()))->getRetType() == TypeSystem::intType){
    //             new RetInstruction(new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)), *block);
    //         }                 
    //     }
    // }


    