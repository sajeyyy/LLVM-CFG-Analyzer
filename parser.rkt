#lang racket

(require "ast.rkt"
         racket/match
         racket/string
         racket/cmdline)

(provide parse-llvm parse-function parse-basic-blocks find-block-by-label output-dot-file build-cfg)


;; Parse an LLVM instruction
(define (parse-llvm line)
  (if (string-blank? line)  ;; Check for empty or blank lines
      #f  ;; Return #f for blank lines, which can be ignored later
      (begin
        (printf "Processing line: ~a\n" line) ;; Log the line being processed

        ;; Ignore lines with just curly braces
        (if (regexp-match? #px"^\\s*[{}]\\s*$" line)
            (printf "Ignoring curly brace: ~a\n" line)  ;; Just log and skip curly brace
            
        ;; First check for each pattern using `regexp-match?`
        (cond
          ;; Match 'alloca' (e.g., "%ptr = alloca i32")
          [(regexp-match? #px"^\\s*(%\\w+) = alloca (\\w+)$" line)
           (printf "Matched alloca!\n")
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = alloca (\\w+)$" line)]
                  [lhs (second matches)]
                  [type (third matches)])
             (printf "Extracted values: lhs=~a, type=~a\n" lhs type)
             (LLVM-Instruction 'alloca (list lhs type)))]

          ;; Match 'getelementpointer' (e.g., "%gep = getelementptr i32, i32* %ptr, i32 0")
          [(regexp-match? #px"^\\s*(%\\w+) = getelementptr (.*)$" line)
           (printf "Matched getelementpointer!\n")
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = getelementptr (.*)$" line)]
                  [lhs (second matches)]
                  [operands (third matches)])
             (printf "Extracted values: lhs=~a, operands=~a\n" lhs operands)
             (LLVM-Instruction 'getelementpointer (list lhs operands)))]

          ;; Match 'load' (e.g., "%val = load i32, i32* %ptr")
          [(regexp-match? #px"^\\s*(%\\w+) = load (.*)$" line)
           (printf "Matched load!\n")
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = load (.*)$" line)]
                  [lhs (second matches)]
                  [operands (third matches)])
             (printf "Extracted values: lhs=~a, operands=~a\n" lhs operands)
             (LLVM-Instruction 'load (list lhs operands)))]

          ;; Match 'store' (e.g., "store i32 %val, i32* %ptr")
          [(regexp-match? #px"^\\s*store (.*)$" line)
           (printf "Matched store!\n")
           (let* ([matches (regexp-match #px"^\\s*store (.*)$" line)]
                  [operands (second matches)])
             (printf "Extracted values: operands=~a\n" operands)
             (LLVM-Instruction 'store (list operands)))]

          ;; Match 'ret' (e.g., "ret i32 %val")
          [(regexp-match? #px"^\\s*ret (.*)$" line)
           (printf "Matched ret!\n")
           (let* ([matches (regexp-match #px"^\\s*ret (.*)$" line)]
                  [operands (second matches)])
             (printf "Extracted values: operands=~a\n" operands)
             (LLVM-Instruction 'ret (list operands)))]

          ;; Match 'icmp' (e.g., "%cmp = icmp eq i32 %argc, 1")
          [(regexp-match? #px"^\\s*(%\\w+) = icmp (\\w+) (\\w+) (%\\w+), (%\\w+|\\d+)$" line)
           (printf "Matched icmp!\n")
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = icmp (\\w+) (\\w+) (%\\w+), (%\\w+|\\d+)$" line)]
                  [lhs (second matches)]
                  [cond (third matches)]
                  [type (fourth matches)]
                  [op1 (fifth matches)]
                  [op2 (sixth matches)])
             (printf "Extracted values: lhs=~a, cond=~a, type=~a, op1=~a, op2=~a\n" lhs cond type op1 op2)
             (LLVM-Instruction 'icmp (list lhs cond type op1 op2)))]

          ;; Match 'br' (e.g., "br i1 %cmp, label %then, label %else")
          [(regexp-match? #px"^\\s*br (.*)$" line)
           (printf "Matched br!\n")
           (let* ([matches (regexp-match #px"^\\s*br (.*)$" line)]
                  [operands (second matches)])
             (printf "Extracted values: operands=~a\n" operands)
             (LLVM-Instruction 'br (list operands)))]

          ;; Match 'sub', 'div', 'mul', 'add'
          [(regexp-match? #px"^\\s*(%\\w+) = (add|sub|div|mul) (\\w+) (%\\w+|\\d+), (%\\w+|\\d+)$" line)
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = (add|sub|div|mul) (\\w+) (%\\w+|\\d+), (%\\w+|\\d+)$" line)]
                  [lhs (second matches)]
                  [opcode (third matches)]
                  [type (fourth matches)]
                  [op1 (fifth matches)]
                  [op2 (sixth matches)])
             (printf "Matched ~a!\n" opcode)
             (printf "Extracted values: lhs=~a, type=~a, op1=~a, op2=~a\n" lhs type op1 op2)
             (LLVM-Instruction opcode (list lhs type op1 op2)))]

          ;; Match 'phi' (e.g., "%res = phi i32 [%val1, %then], [%val2, %else]")
          [(regexp-match? #px"^\\s*(%\\w+) = phi (.*)$" line)
           (printf "Matched phi!\n")
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = phi (.*)$" line)]
                  [lhs (second matches)]
                  [operands (third matches)])
             (printf "Extracted values: lhs=~a, operands=~a\n" lhs operands)
             (LLVM-Instruction 'phi (list lhs operands)))]

          ;; Match 'call' (e.g., "%result = call i32 @func(i32 %a)")
          [(regexp-match? #px"^\\s*(%\\w+) = call (.*)$" line)
           (printf "Matched call!\n")
           (let* ([matches (regexp-match #px"^\\s*(%\\w+) = call (.*)$" line)]
                  [lhs (second matches)]
                  [operands (third matches)])
             (printf "Extracted values: lhs=~a, operands=~a\n" lhs operands)
             (LLVM-Instruction 'call (list lhs operands)))]

          ;; Match 'define' (e.g., "define i32 @main(i32 %argc)")
          [(regexp-match? #px"^\\s*define (.*)$" line)
           (printf "Matched define!\n")
           (let* ([matches (regexp-match #px"^\\s*define (.*)$" line)]
                  [signature (second matches)])
             (printf "Extracted signature: ~a\n" signature)
             (LLVM-Instruction 'define (list signature)))]

          ;; Handle unknown instruction with error logging
          [else
           (begin
             (printf "Failed to match instruction: ~a\n" line) ;; Log the failed line
             (error "Unknown instruction or format: ~a" line))])))))

(define (string-blank? line)
  (or (not line) (regexp-match? #px"^\\s*$" line)))

;; Parse basic blocks within a function body
(define (parse-basic-blocks lines)
  (define blocks '())
  (define current-block-label #f)
  (define current-instructions '())

  ;; Helper to finalize the current block
  (define (finalize-block)
    (when current-block-label
      (set! blocks (cons (BasicBlock current-block-label current-instructions '()) blocks))
      (set! current-block-label #f)
      (set! current-instructions '())))

  ;; Process each line
  (for ([line lines])
    (printf "Processing line in basic block: ~a\n" line)
    (cond
      ;; If the line is a label (start of a block)
      [(regexp-match #px"^(\\w+):$" line)
       (finalize-block)
       (set! current-block-label (car (regexp-match #px"^(\\w+):$" line)))]
      
      ;; If it's a control flow instruction (end of a block)
      [(regexp-match #px"^(br|ret)\\b" line)
       (define instruction (parse-llvm line))
       (when instruction (set! current-instructions (cons instruction current-instructions)))
       (finalize-block)]

      ;; For other instructions
      [else
       (define instruction (parse-llvm line))
       (when instruction (set! current-instructions (cons instruction current-instructions)))]))

  ;; Finalize the last block
  (finalize-block)
  (reverse blocks))

;; Find a basic block by its label in a list of blocks
(define (find-block-by-label label blocks)
  (define found-block
    (for/fold ([result #f]) ([block blocks])
      (if (equal? label (BasicBlock-label block))
          block
          result)))
  (if found-block
      found-block
      (error "Basic block with label ~a not found" label)))

;; Parse a single function from its lines
(define (parse-function lines)
  ;; Check that lines are not empty to avoid errors
  (if (null? lines)
      (error "No lines to parse.")
      (let* ([function-name (car lines)]        ;; The first line contains the function definition
             [function-body (cdr lines)]        ;; The remaining lines contain the function body
             [basic-blocks (parse-basic-blocks function-body)]) ;; Parse the basic blocks from the body
        ;; Return a Function struct with the parsed name and basic blocks
        (Function function-name basic-blocks))))

;; Build control-flow graph (CFG) from basic blocks
(define (build-cfg function)
  (for ([block (Function-basic-blocks function)])
    (define successors '())
    (define last-inst (car (reverse (BasicBlock-instructions block))))

    (match last-inst
      [(LLVM-Instruction 'br operands)
       (for ([label operands])
         (set! successors (cons (find-block-by-label label (Function-basic-blocks function)) successors)))]
      [(LLVM-Instruction 'ret _) 
       (set! successors '())]
      [_ #f])
    (set-BasicBlock-successors! block successors))
  function)

;; Generate Graphviz dot content from CFG
(define (generate-dot-content function)
  (define dot-output "digraph {\n")

  ;; For each basic block
  (for ([block (Function-basic-blocks function)])
    (define node-id (BasicBlock-label block))
    (set! dot-output (string-append dot-output (format "    ~a [shape=record,label=\"\"]\n" node-id)))

    ;; Add edges to successor blocks
    (for ([succ (BasicBlock-successors block)] [i (in-naturals)])
      (set! dot-output (string-append dot-output
                                      (format "    ~a -> ~a [label=~a];\n" node-id (BasicBlock-label succ) i)))))

  ;; Close the digraph
  (set! dot-output (string-append dot-output "}\n"))

  dot-output)

;; Output the CFG in Graphviz dot format to a file
(define (output-dot-file filename function)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out (generate-dot-content function)))
    #:exists 'replace))
