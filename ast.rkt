#lang racket

(provide
  (struct-out LLVM-Instruction)
  (struct-out BasicBlock)
  (struct-out Function))

;; Define our AST Structure Nodes

(struct LLVM-Instruction (opcode operands) #:transparent) ;; Structure of an LLVM instruction
(struct BasicBlock (label instructions successors) #:transparent #:mutable) ;; Structure of a basic block
(struct Function (name basic-blocks) #:transparent) ;;Structure of a function containing basic blocks
