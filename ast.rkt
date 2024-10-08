#lang racket

(provide
  (struct-out LLVM-Instruction)
  (struct-out BasicBlock)
  (struct-out Function))

;; Define the structure of an LLVM instruction
(struct LLVM-Instruction (opcode operands) #:transparent)

;; Define the structure of a basic block
(struct BasicBlock (label instructions successors) #:transparent #:mutable)

;; Define the structure of a function containing basic blocks
(struct Function (name basic-blocks) #:transparent)
