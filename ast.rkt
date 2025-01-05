#lang racket

;; Provide our structures so other files have access
(provide
  (struct-out LLVM-Instruction)
  (struct-out BasicBlock)
  (struct-out Function)
  (struct-out GlobalVariable)
  (struct-out ExternalFunction))

;; Define our AST Structure Nodes
(struct LLVM-Instruction (opcode operands) #:transparent) ;; Structure of an LLVM instruction
(struct BasicBlock (label instructions successors) #:transparent #:mutable) ;; Structure of a basic block
(struct Function (name basic-blocks) #:transparent) ;; Structure of a function containing basic blocks
(struct GlobalVariable (name type value align) #:transparent) ;; Structure of a global variable
(struct ExternalFunction (name ret-type params) #:transparent) ;; Structure of an external function
