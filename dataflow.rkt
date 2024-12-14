#lang racket

(require "ast.rkt"
         "parser.rkt"
         racket/cmdline
         racket/match
         racket/string)

(provide dataflow-analyze)

;; A simple queue
(define (make-queue) '())
(define (queue-empty? q) (null? q))
(define (queue-enqueue! q item) (append q (list item)))
(define (queue-dequeue! q) (values (car q) (cdr q)))

;; Helpers for taint analysis
(define (is-register? v) (and (string? v) (regexp-match? #px"^%\\w+" v)))
(define (is-global? v) (and (string? v) (regexp-match? #px"^@\\w+" v)))

;; Extract memory key from operands like "ptr %aVar" or "ptr @a"
(define (memory-key-from-operand op)
  (cond
    [(regexp-match #px".*ptr\\s+(@\\w+).*" op) => (λ(m)(second m))]
    [(regexp-match #px".*ptr\\s+(%\\w+).*" op) => (λ(m)(second m))]
    [else #f]))

(define (operand-tainted? op registers memory)
  ;; If op is a register
  (cond
    [(is-register? op) (hash-ref registers op #f)]
    [(regexp-match? #px"^\\d+$" op) #f] ;; numeric constant not tainted
    [else #f]))

;; load: taint of loaded register depends on memory
(define (load-taint op registers memory)
  (define memkey (memory-key-from-operand op))
  (if memkey
      (hash-ref memory memkey #f)
      #f))

;; store: taint memory based on value
(define (store-taint val-op addr-op registers memory)
  ;; val-op might be "0" or "%1"
  (define val-taint
    (if (is-register? val-op)
        (hash-ref registers val-op #f)
        (if (regexp-match? #px"^\\d+$" val-op) #f #f))) ;; Immediate constants untainted
  (define memkey (memory-key-from-operand addr-op))
  (when memkey (hash-set! memory memkey val-taint))
  (values registers memory))

;; Arithmetic or icmp: if any operand is tainted => result tainted
(define (arith-taint lhs op1 op2 registers memory)
  (define t1 (operand-tainted? op1 registers memory))
  (define t2 (operand-tainted? op2 registers memory))
  (hash-set! registers lhs (or t1 t2))
  (values registers memory))

;; phi node: if any incoming value is tainted, phi result is tainted
(define (phi-taint lhs ops registers memory)
  ;; ops example: "i32 [%val1, %then], [%val2, %else]"
  ;; We split by '],' to get pairs
  (define raw (regexp-replace* #px"\\[|\\]" ops "")) ;; remove brackets
  ;; Now raw like: "i32 %val1, %then, %val2, %else"
  ;; The pattern is type val,label pairs
  (define parts (map string-trim (string-split raw ",")))
  ;; parts might be ("i32 %val1" "%then" "%val2" "%else")
  (define tainted #f)
  (for ([i (in-range 0 (length parts) 2)] #:when (< (+ i 1) (length parts)))
    (define val-line (list-ref parts i))
    ;; val-line might have type and val
    (define val-toks (string-split val-line #px"\\s+"))
    (define val (last val-toks))
    (when (is-register? val)
      (when (hash-ref registers val #f)
        (set! tainted #t))))
  (hash-set! registers lhs tainted)
  (values registers memory))

;; parse-call-fallback if needed
(define (parse-call-fallback op-string)
  (define-values (callee args) (parse-call op-string))
  (values callee args))

;; handle SOURCE and SINK inside calls:
;; SOURCE: result tainted if lhs present.
(define (handle-call lhs rtype fname args registers memory)
  (cond
    [(string=? fname "SOURCE")
     (when lhs (hash-set! registers lhs #t))
     (values registers memory)]
    [else
     ;; Normal calls do not change taint unless it's SOURCE
     (values registers memory)]))

;; Handle instructions with match
(define (handle-instruction inst registers memory)
  (match inst
    ;; load
    [(LLVM-Instruction 'load (list lhs opstring align))
     (define val-taint (load-taint opstring registers memory))
     (hash-set! registers lhs val-taint)
     (values registers memory)]

    ;; store
    [(LLVM-Instruction 'store (list opstring align))
     (define parts (map string-trim (string-split opstring ",")))
     (define val-part (car parts))
     (define addr-part (cadr parts))
     (define val-toks (string-split val-part #px"\\s+"))
     (define val (last val-toks))
     (store-taint val addr-part registers memory)]

    ;; arithmetic
    [(LLVM-Instruction opcode (list lhs type op1 op2))
     #:when (member opcode '(add sub mul div))
     (arith-taint lhs op1 op2 registers memory)]

    ;; icmp
    [(LLVM-Instruction 'icmp (list lhs cond type op1 op2))
     (arith-taint lhs op1 op2 registers memory)]

    ;; phi
    [(LLVM-Instruction 'phi (list lhs ops))
     (phi-taint lhs ops registers memory)]

    ;; call
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

;; Check if a SINK call is called with tainted arg
(define (sink-called-with-taint? inst registers memory)
  (match inst
    [(LLVM-Instruction 'call (list lhs rtype fname args))
     (and (string=? fname "SINK")
          (for/or ([arg (map string-trim (string-split args ","))])
            (let* ([tokens (string-split arg #px"\\s+")]
                   [last-token (car (reverse tokens))])
              (and (is-register? last-token)
                   (hash-ref registers last-token #f)))))]
    [_ #f]))

(define (dataflow-analyze function)
  ;; Initialization
  (define initial-registers (make-hash))
  (define initial-memory (make-hash))
  (define worklist (make-queue))
  (for ([bb (Function-basic-blocks function)])
    (when (string=? (BasicBlock-label bb) "entry")
      (set! worklist (queue-enqueue! worklist bb))))
  (define detected-flow? #f)
  (define block-states (make-hash))
  (for ([bb (Function-basic-blocks function)])
    (hash-set! block-states bb (cons (hash-copy initial-registers) (hash-copy initial-memory))))

  ;; Helper functions
  (define (merge-states old-state new-state)
    (define old-reg (car old-state))
    (define old-mem (cdr old-state))
    (define new-reg (car new-state))
    (define new-mem (cdr new-state))
    (for ([k (hash-keys new-reg)])
      (when (hash-ref new-reg k #f) (hash-set! old-reg k #t)))
    (for ([k (hash-keys new-mem)])
      (when (hash-ref new-mem k #f) (hash-set! old-mem k #t)))
    (cons old-reg old-mem))

  (define (analyze-block block reg mem)
    (for ([inst (BasicBlock-instructions block)])
      (define-values (new-reg new-mem) (handle-instruction inst reg mem))
      (set! reg new-reg)
      (set! mem new-mem)
      (when (sink-called-with-taint? inst reg mem)
        (set! detected-flow? #t)))
    (values reg mem))

  ;; Worklist algorithm
  (let loop ([wl worklist])
    (unless (queue-empty? wl)
      (define-values (block rest-wl) (queue-dequeue! wl))
      (define state (hash-ref block-states block))
      (define old-reg (car state))
      (define old-mem (cdr state))
      (define-values (new-reg new-mem) (analyze-block block (hash-copy old-reg) (hash-copy old-mem)))
      (for ([succ (BasicBlock-successors block)])
        (define succ-state (hash-ref block-states succ))
        (define merged (merge-states succ-state (cons (hash-copy new-reg) (hash-copy new-mem))))
        (unless (and (equal? (hash-keys (car succ-state)) (hash-keys (car merged)))
                     (equal? (hash-keys (cdr succ-state)) (hash-keys (cdr merged))))
          (hash-set! block-states succ merged)
          (set! rest-wl (queue-enqueue! rest-wl succ))))
      (loop rest-wl)))
  detected-flow?)
