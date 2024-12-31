#lang racket

(require "ast.rkt"
         "parser.rkt"
         racket/cmdline
         racket/match
         racket/string)

;Export function
(provide dataflow-analyze)

;; Define variables
(define detected-flow? #f)
(define block-states (make-hash))


;; Queue Implementation
(define (make-queue) '())
(define (queue-empty? q) (null? q))
(define (queue-enqueue! q item) (append q (list item)))
(define (queue-dequeue! q) (values (car q) (cdr q)))


;; Helpers for the taint analysis
(define (is-register? v) (and (string? v) (regexp-match? #px"^%\\w+" v))) ;; Checks if v is a register
(define (is-global? v) (and (string? v) (regexp-match? #px"^@\\w+" v))) ;; Checks if V is a global variable


;; Extract the memory key from 'ptr' instructions
(define (memory-key-from-operand op)
  (cond
    [(regexp-match #px".*ptr\\s+(@\\w+).*" op) => (λ(m)(second m))]
    [(regexp-match #px".*ptr\\s+(%\\w+).*" op) => (λ(m)(second m))]
    [else #f]))


;;Check if operands are tainted in registers or memeory
(define (operand-tainted? op registers memory)
  (cond
    [(is-register? op) (hash-ref registers op #f)] ;; Check taint in regisers
    [(regexp-match? #px"^\\d+$" op) #f] ;; If constant, not tainted
    [else #f])) ;; Else not tainted


;; Load instruction
(define (load-taint op registers memory)
  (define memkey (memory-key-from-operand op)) ;; Get memory key
  (if memkey
      (hash-ref memory memkey #f) ;; Return the state of taint
      #f)) ;; Default


;; Store Instruction
(define (store-taint val-op addr-op registers memory)
  (define val-taint
    (if (is-register? val-op)
        (hash-ref registers val-op #f)
        (if (regexp-match? #px"^\\d+$" val-op) #f #f))) ;; Constants untainted
  (define memkey (memory-key-from-operand addr-op)) ;; Get memory key
  (when memkey (hash-set! memory memkey val-taint)) ;; Update the taint state
  (values registers memory))


;; Arithmeic and Comparisons
(define (arith-taint lhs op1 op2 registers memory)
  ;; Check both operands
  (define t1 (operand-tainted? op1 registers memory))
  (define t2 (operand-tainted? op2 registers memory))
  (hash-set! registers lhs (or t1 t2)) ;; Tainted if either operand is tainted
  (values registers memory))


;; Phi
(define (phi-taint lhs ops registers memory)
  (define raw (regexp-replace* #px"\\[|\\]" ops "")) ;; Remove brackets
  (define parts (map string-trim (string-split raw ","))) ;; Split the operands into parts
  (define tainted #f)

  ;; Iterate through each pair checking if any operand is tainted
  (for ([i (in-range 0 (length parts) 2)] #:when (< (+ i 1) (length parts)))
    (define val-line (list-ref parts i))
    (define val-toks (string-split val-line #px"\\s+"))
    (define val (last val-toks)) ;; Get value
    (when (is-register? val)
      (when (hash-ref registers val #f) ;;Check if val is tainted
        (set! tainted #t))))
  (hash-set! registers lhs tainted) ;; Update the taint state
  (values registers memory))


;; Check if the taint state has changed for registers or memory
(define (state-changed? old-state new-state)
  (define old-reg (car old-state))
  (define old-mem (cdr old-state))
  (define new-reg (car new-state))
  (define new-mem (cdr new-state))

  ;; Helper to compare the 2 hash tables
  (define (hashes-differ? h1 h2)
    (or (not (equal? (hash-keys h1) (hash-keys h2)))
        (for/or ([k (hash-keys h1)])
          (not (equal? (hash-ref h1 k #f) (hash-ref h2 k #f))))))
  ;; Compare are return results
  (or (hashes-differ? old-reg new-reg)
      (hashes-differ? old-mem new-mem)))


;; handle SOURCE and SINK calls
(define (handle-call lhs rtype fname args registers memory)
  (cond
    [(string=? fname "SOURCE") ;; If source, taint true
     (when lhs (hash-set! registers lhs #t))
     (values registers memory)]
    [else ;; Normal calls do not change taint
     (values registers memory)]))


;; Handle instructions with match statements, and call the respective helper function to handle that instruction
(define (handle-instruction inst registers memory)
  (match inst
    ;; Load
    [(LLVM-Instruction 'load (list lhs opstring align))
     (define val-taint (load-taint opstring registers memory))
     (hash-set! registers lhs val-taint)
     (values registers memory)]

    ;; Store
    [(LLVM-Instruction 'store (list opstring align))
     (define parts (map string-trim (string-split opstring ",")))
     (define val-part (car parts))
     (define addr-part (cadr parts))
     (define val-toks (string-split val-part #px"\\s+"))
     (define val (last val-toks))
     (store-taint val addr-part registers memory)]

    ;; Arithmetic
    [(LLVM-Instruction opcode (list lhs type op1 op2))
     #:when (member opcode '(add sub mul div))
     (arith-taint lhs op1 op2 registers memory)]

    ;; Icmp
    [(LLVM-Instruction 'icmp (list lhs cond type op1 op2))
     (arith-taint lhs op1 op2 registers memory)]

    ;; Phi
    [(LLVM-Instruction 'phi (list lhs ops))
     (phi-taint lhs ops registers memory)]

    ;; Call
    [(LLVM-Instruction 'call (list lhs rtype fname args))
     (handle-call lhs rtype fname (map string-trim (string-split args ",")) registers memory)]

    ;; call fallback
    [(LLVM-Instruction 'call (list lhs rest))
     (define-values (callee args) (parse-call rest))
     (handle-call lhs "unknown" callee args registers memory)]

    ;; ret
    [(LLVM-Instruction 'ret (list value))
     (values registers memory)]

    ;; Default case
    [_ (values registers memory)]))


;; Check if a SINK call is called with tainted argument
(define (sink-called-with-taint? inst registers memory)
  ;; Make sure it's a call instruction with sink
  (match inst
    [(LLVM-Instruction 'call (list lhs rtype fname args))
     ;; If functinon name sink, iterate throught the arguements to determine if any are tainted
     (and (string=? fname "SINK")
          (for/or ([arg (map string-trim (string-split args ","))]) ;; Split args into lists
            (let* ([tokens (string-split arg #px"\\s+")]
                   [last-token (car (reverse tokens))]) ;; Get last arg, either a register or value
              (and (is-register? last-token)
                   (hash-ref registers last-token #f)))))] ;; If register, check if tainted
    [_ #f])) ;; Else return false


;; Helper function to merge old and new states
(define (merge-states old-state new-state)
    ;; Get register and memory from states
    (define old-reg (car old-state))
    (define old-mem (cdr old-state))
    (define new-reg (car new-state))
    (define new-mem (cdr new-state))

    ;; Iterate through merging old registers with tainted registers
    (for ([k (hash-keys new-reg)])
      (when (hash-ref new-reg k #f) (hash-set! old-reg k #t)))
    ;; ;; Iterate through merging old memory with tainted memory
    (for ([k (hash-keys new-mem)])
      (when (hash-ref new-mem k #f) (hash-set! old-mem k #t)))
    (cons old-reg old-mem)) ;; Return merged state


;; Dataflow analysis over a single block
(define (analyze-block block reg mem)
    ;; Iterate through all instruction in block
    (for ([inst (BasicBlock-instructions block)])
      ;; Handle instruction and update the register/memory states
      (define-values (new-reg new-mem) (handle-instruction inst reg mem))
      (set! reg new-reg)
      (set! mem new-mem)
      ;;Check if instruction is sink call
      (when (sink-called-with-taint? inst reg mem)
        (set! detected-flow? #t)))
    (values reg mem)) ;; Return states after analyzing the block


;; Main Dataflow Analysis Function
(define (dataflow-analyze function)
  ;; Initialize empty register/memory states
  (define initial-registers (make-hash))
  (define initial-memory (make-hash))
  (define worklist (make-queue)) ;; Create worklist using a queue

  ;; Enqueue all blocks initially
  (for ([bb (Function-basic-blocks function)])
    (set! worklist (queue-enqueue! worklist bb)))

  ;; Initialize block states
  (for ([bb (Function-basic-blocks function)])
    (hash-set! block-states bb (cons (hash-copy initial-registers) (hash-copy initial-memory))))

  ;; Worklist algorithm:
  (let loop ([wl worklist]) ;; Iterate through worklist until empty
    (unless (queue-empty? wl)
      (define-values (block rest-wl) (queue-dequeue! wl)) ;; Dequeue to process
      ;;Ge current state of blocks
      (define state (hash-ref block-states block))
      (define old-reg (car state))
      (define old-mem (cdr state))

      ;; Analyze current block and compute the new states
      (define-values (new-reg new-mem) (analyze-block block (hash-copy old-reg) (hash-copy old-mem)))

      ;;Iterate over successors
      (for ([succ (BasicBlock-successors block)])
        (define succ-state (hash-ref block-states succ)) ;; Get states of successors
        (define merged (merge-states succ-state (cons (hash-copy new-reg) (hash-copy new-mem)))) ;; Merge states
        (hash-set! block-states succ merged) ;; Enqueue successor
        (set! rest-wl (queue-enqueue! rest-wl succ))) ;; Update the state and enqueue
      (loop rest-wl))) ;; Loop
  detected-flow?) ;; Return whether a taint was found
