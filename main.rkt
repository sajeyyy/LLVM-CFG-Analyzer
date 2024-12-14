#lang racket

(require "parser.rkt"
         "ast.rkt"
         "dataflow.rkt"
         racket/cmdline
         racket/match
         racket/string)

;; Build control-flow graph from all of the basic blocks
(define (build-cfg function)
  (for ([block (Function-basic-blocks function)])
    (define instructions (BasicBlock-instructions block))
    (define successors '())
    (if (null? instructions)
        (void)
        (let ([last-inst (last instructions)])
          (match last-inst
            [(LLVM-Instruction 'br operands)
             (cond
               [(= (length operands) 4)
                (define label-true (substring (list-ref operands 2) 1))
                (define label-false (substring (list-ref operands 3) 1))
                (define succ-block-true (find-block-by-label label-true (Function-basic-blocks function)))
                (define succ-block-false (find-block-by-label label-false (Function-basic-blocks function)))
                (when succ-block-true (set! successors (cons succ-block-true successors)))
                (when succ-block-false (set! successors (cons succ-block-false successors)))]
               [(= (length operands) 1)
                (define label (substring (first operands) 1))
                (define succ-block (find-block-by-label label (Function-basic-blocks function)))
                (when succ-block (set! successors (cons succ-block successors)))]
               [else (void)])]
            [(LLVM-Instruction 'ret _)
             (set! successors '())]
            [_ (void)])))
    (set-BasicBlock-successors! block (reverse successors)))
  function)

(define (main)
  (define input-file
    (command-line
     #:program "dataflow"
     #:args (input)
     "Provide the LLVM IR input file."
     input))

  (define lines
    (with-input-from-file input-file
      (lambda () (port->lines (current-input-port)))))

  ;; Parse entire LLVM file
  (define parsed (parse-llvm-file lines))
  (define functions (first parsed))
  (define externals (second parsed))
  (define globals (third parsed))

  (for ([func functions])
    (define cfg-func (build-cfg func))
    (define flow? (dataflow-analyze cfg-func))
    (if flow?
        (printf "FLOW\n")
        (printf "NO FLOW\n"))))

(main)
