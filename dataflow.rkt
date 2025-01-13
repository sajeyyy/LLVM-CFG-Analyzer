#lang racket

(require "ast.rkt"
         racket/match
         racket/string)

(provide dataflow-analyze)

(define (dataflow-analyze function)
  ;; block-in/out are hash tables: block -> set-of-strings
  (define block-in (make-hash))
  (define block-out (make-hash))

  (define blocks (Function-basic-blocks function))
  (for ([b blocks])
    (hash-set! block-in b '())
    (hash-set! block-out b '()))

  ;; trivial "transfer function": each instruction that "defines" a register
  ;; gets added to the out set
  (define (transfer block in-set)
    (define out-set in-set)
    (for ([inst (BasicBlock-instructions block)])
      (match inst
        [(LLVM-Instruction 'load (list lhs opstring _align))
         (set! out-set (cons (format "def(~a)" lhs) out-set))]
        [(LLVM-Instruction opcode (list lhs _ op1 op2))
         #:when (member opcode '(add sub mul div))
         (set! out-set (cons (format "def(~a)" lhs) out-set))]
        [_ #f]))
    out-set)

  (define (join sets) (apply append sets))

  ;; gather preds
  (define (preds-of blk)
    (for/list ([pb blocks]
               #:when (member blk (BasicBlock-successors pb)))
      pb))

  (define worklist blocks)

  (let loop ()
    (unless (null? worklist)
      (define b (car worklist))
      (define tail (cdr worklist))

      (define new-in (join (map (λ(x) (hash-ref block-out x '()))
                                (preds-of b))))
      (define old-in (hash-ref block-in b))
      (when (not (equal? old-in new-in))
        (hash-set! block-in b new-in))

      (define new-out (transfer b new-in))
      (define old-out (hash-ref block-out b))
      (when (not (equal? old-out new-out))
        (hash-set! block-out b new-out)
        ;; re-enqueue successors
        (for ([s (BasicBlock-successors b)])
          (set! tail (cons s tail))))

      (set! worklist tail)
      (loop)))

  ;; Return 2 lambdas so the caller can do (block-in block) -> set
  (values
   (λ(b) (hash-ref block-in b '()))
   (λ(b) (hash-ref block-out b '()))))
