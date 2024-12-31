#lang racket

;; Include external files
(require "parser.rkt"
         "ast.rkt"
         "dataflow.rkt"
         racket/cmdline
         racket/match
         racket/string)

;; Build control-flow graph from all of the basic blocks in a given function
(define (build-cfg function)
  (for ([block (Function-basic-blocks function)])
    (define instructions (BasicBlock-instructions block)) ;; Contains all block instructions
    (define successors '()) ;; Contains successors of block

    ;;Check if block has instructions
    (if (null? instructions)
        (void) ;; If no instructions, do nothing
        (let ([last-inst (last instructions)]) ;; Else, analyze the last instruction in order to find successor
          (match last-inst
            [(LLVM-Instruction 'br operands)
             (cond
               ;; Conditional Branch
               [(= (length operands) 4)
                ;; Extract true and false labels from operands
                (define label-true (substring (list-ref operands 2) 1))
                (define label-false (substring (list-ref operands 3) 1))
                ;; Find successor blocks by their labels
                (define succ-block-true (find-block-by-label label-true (Function-basic-blocks function)))
                (define succ-block-false (find-block-by-label label-false (Function-basic-blocks function)))
                ;; Add successor blocks
                (when succ-block-true (set! successors (cons succ-block-true successors)))
                (when succ-block-false (set! successors (cons succ-block-false successors)))]

               ;; Unconditional Branch
               [(= (length operands) 1)
                ;; Get destination label
                (define label (substring (first operands) 1))
                ;; Find successor blocks by it's label
                (define succ-block (find-block-by-label label (Function-basic-blocks function)))
                ;; Add successor block
                (when succ-block (set! successors (cons succ-block successors)))]
               [else (void)])]
            [(LLVM-Instruction 'ret _) ;; Return instruction
             (set! successors '())]
            [_ (void)]))) ;; Else, other instructions do not affect control flow

    (set-BasicBlock-successors! block (reverse successors))) ;; Update the block with it's successors
  function) ;; Return the modified function with CFG information


;; Main Function
(define (main)
  ;; Parse the command line arguement to get file name
  (define input-file
    (command-line
     #:program "dataflow"
     #:args (input)
     "Provide the LLVM IR input file."
     input))

  ;; Parse all lines from the provided file
  (define lines
    (with-input-from-file input-file
      (lambda () (port->lines (current-input-port)))))

  ;; Parse the entire LLVM file into lists of functions, externals, and globals
  (define parsed (parse-llvm-file lines))
  (define functions (first parsed))
  (define externals (second parsed))
  (define globals (third parsed))

  ;; Iterate through each function, build its CFG and analyze the dataflow of the CFG
  (for ([func functions])
    (define cfg-func (build-cfg func))
    (define flow? (dataflow-analyze cfg-func))

    ;; Print whether a dataflow is detected or not
    (if flow?
        (printf "\nFLOW\n\n")
        (printf "\nNO FLOW\n\n"))))

(main)
