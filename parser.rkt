#lang racket

(require "ast.rkt"
         racket/match
         racket/string
         racket/cmdline)

(provide parse-llvm parse-function parse-basic-blocks find-block-by-label)


;; Check if a line is blank or whitespace
(define (line-blank? line)
  (or (not line) (regexp-match? #px"^\\s*$" line)))

;; Parses a LLVM instruction using regular expressions and pattern matching
(define (parse-llvm line)
  (if (line-blank? line)
      #f
      (begin
        (if (regexp-match? #px"^\\s*[{}]\\s*$" line)
            #f
            (cond
              ;; Global variable
              [(regexp-match? #px"^\\s*@(\\w+)\\s*=\\s*global\\s+(\\w+)\\s+([\\w\\d]+)(,\\s*align\\s+(\\d+))?\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*@(\\w+)\\s*=\\s*global\\s+(\\w+)\\s+([\\w\\d]+)(,\\s*align\\s+(\\d+))?\\s*$" line)]
                      [name (second m)]
                      [type (third m)]
                      [value (fourth m)]
                      [align-str (fifth m)]
                      [align (if align-str (string->number align-str) #f)])
                 (GlobalVariable name type value align))]

               ;; Define (start of a function)
               [(regexp-match? #px"^\\s*define\\s+(?:dso_local\\s+)?(\\w+)\\s+@([\\w.]+)\\((.*)\\)\\s*(#[\\d]+)?\\s*\\{?$" line)
                (let* ([m (regexp-match #px"^\\s*define\\s+(?:dso_local\\s+)?(\\w+)\\s+@([\\w.]+)\\((.*)\\)\\s*(#[\\d]+)?\\s*\\{?$" line)]
                       [ret-type (second m)]
                       [fname (third m)]
                       [params (fourth m)]
                       [metadata (fifth m)])
                  (LLVM-Instruction 'define (list ret-type fname params metadata)))]

              ;; Function declaration (External Function)
              [(regexp-match? #px"^\\s*declare\\s+(?:dso_local\\s+)?(\\w+)\\s+@([\\w.]+)\\((.*)\\)\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*declare\\s+(?:dso_local\\s+)?(\\w+)\\s+@([\\w.]+)\\((.*)\\)\\s*$" line)]
                      [ret-type (second m)]
                      [fname (third m)]
                      [params (fourth m)])
                 (ExternalFunction fname ret-type params))]

              ;; Load
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*load\\s+(.*?)(,\\s*align\\s+(\\d+))?\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*load\\s+(.*?)(,\\s*align\\s+(\\d+))?\\s*$" line)]
                      [lhs (second m)]
                      [opstring (third m)]
                      [align-str (fifth m)]
                      [align (if align-str (string->number align-str) #f)])
                 (LLVM-Instruction 'load (list lhs opstring align)))]

              ;; Store
              [(regexp-match? #px"^\\s*store\\s+(.*?)(,\\s*align\\s+(\\d+))?\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*store\\s+(.*?)(,\\s*align\\s+(\\d+))?\\s*$" line)]
                      [opstring (second m)]
                      [align-str (fourth m)]
                      [align (if align-str (string->number align-str) #f)])
                 (LLVM-Instruction 'store (list opstring align)))]

              ;; Return
              [(regexp-match? #px"^\\s*ret\\s+(.*)$" line)
               (let* ([m (regexp-match #px"^\\s*ret\\s+(.*)$" line)]
                      [ops (second m)])
                 (LLVM-Instruction 'ret (list ops)))]

              ;; Branch (Conditional)
              [(regexp-match? #px"^\\s*br\\s+(i\\d+)\\s+(%\\w+),\\s+label\\s+(%\\w+),\\s+label\\s+(%\\w+)$" line)
               (let* ([m (regexp-match #px"^\\s*br\\s+(i\\d+)\\s+(%\\w+),\\s+label\\s+(%\\w+),\\s+label\\s+(%\\w+)$" line)]
                      [cond-type (second m)]
                      [cond-var (third m)]
                      [ltrue (fourth m)]
                      [lfalse (fifth m)])
                 (LLVM-Instruction 'br (list cond-type cond-var ltrue lfalse)))]

              ;; Branch (Unconditional)
              [(regexp-match? #px"^\\s*br\\s+label\\s+(%\\w+)$" line)
               (let* ([m (regexp-match #px"^\\s*br\\s+label\\s+(%\\w+)$" line)]
                      [lbl (second m)])
                 (LLVM-Instruction 'br (list lbl)))]

              ;; Comparison Functions
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*icmp\\s+(\\w+)\\s+(\\w+)\\s+(%\\w+),\\s+(%\\w+|\\d+)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*icmp\\s+(\\w+)\\s+(\\w+)\\s+(%\\w+),\\s+(%\\w+|\\d+)$" line)]
                      [lhs (second m)]
                      [cond (third m)]
                      [type (fourth m)]
                      [op1 (fifth m)]
                      [op2 (sixth m)])
                 (LLVM-Instruction 'icmp (list lhs cond type op1 op2)))]

              ;; Artihmetic: add, sub, mul, div
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*(add|sub|div|mul)\\s+(\\w+)\\s+(%\\w+|\\d+),\\s+(%\\w+|\\d+)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*(add|sub|div|mul)\\s+(\\w+)\\s+(%\\w+|\\d+),\\s+(%\\w+|\\d+)$" line)]
                      [lhs (second m)]
                      [opcode (third m)]
                      [type (fourth m)]
                      [op1 (fifth m)]
                      [op2 (sixth m)])
                 (LLVM-Instruction (string->symbol opcode) (list lhs type op1 op2)))]

              ;; Phi
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*phi\\s+(.*)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*phi\\s+(.*)$" line)]
                      [lhs (second m)]
                      [ops (third m)])
                 (LLVM-Instruction 'phi (list lhs ops)))]

              ;; Call (With function and Arguements)
              [(regexp-match? #px"^\\s*(?:(%\\w+)\\s*=\\s*)?call\\s+(\\w+)\\s+@([\\w.]+)\\((.*)\\)\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*(?:(%\\w+)\\s*=\\s*)?call\\s+(\\w+)\\s+@([\\w.]+)\\((.*)\\)\\s*$" line)]
                      [lhs (second m)]
                      [rtype (third m)]
                      [fname (fourth m)]
                      [args (fifth m)])
                 (LLVM-Instruction 'call (list lhs rtype fname args)))]

              ;; Call
              [(regexp-match? #px"^\\s*(?:(%\\w+)\\s*=\\s*)?call\\s+(.*)$" line)
               (let* ([m (regexp-match #px"^\\s*(?:(%\\w+)\\s*=\\s*)?call\\s+(.*)$" line)]
                      [lhs (second m)]
                      [rest (third m)])
                 (LLVM-Instruction 'call (list lhs rest)))]

              ;; Alloca
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*alloca\\s+(\\w+)(,\\s*align\\s+(\\d+))?\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*alloca\\s+(\\w+)(,\\s*align\\s+(\\d+))?\\s*$" line)]
                      [lhs (second m)]
                      [type (third m)]
                      [align-str (fifth m)]
                      [align (if align-str (string->number align-str) #f)])
                 (LLVM-Instruction 'alloca (list lhs type align)))]

              ;; GetElementPtr
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*getelementptr\\s+(.*)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*getelementptr\\s+(.*)$" line)]
                      [lhs (second m)]
                      [operands (third m)])
                 (LLVM-Instruction 'getelementptr (list lhs operands)))]

              ;; Unsupported/Unknown Instruction
              [else
               (error "Unknown instruction or format: ~a" line)])))))


;; Parse basic blocks within a function body
(define (parse-basic-blocks lines)
  (define blocks '())
  (define current-block-label #f)
  (define current-instructions '())

  ;; Helper to finalize the current block
  (define (finalize-block)
    (when (and current-block-label (not (null? current-instructions)))
      (set! blocks (cons (BasicBlock current-block-label (reverse current-instructions) '()) blocks)))
    ;; Reset the state variables outside the 'when' block
    (set! current-block-label #f)
    (set! current-instructions '()))

  ;; Process each line
  (for ([line lines])
    (cond
      ;; If the line is a label (start of a block)
      [(regexp-match #px"^(\\w+):$" line)
       (finalize-block) ;; Finalize the previous block
       (set! current-instructions '()) ;; Reset instructions for the new block
       (set! current-block-label (second (regexp-match #px"^(\\w+):$" line)))]

      ;; If it's a control flow instruction (end of a block)
      [(regexp-match #px"^\\s*(br|ret)\\b" line)
       (define instruction (parse-llvm line))
       (when (and instruction (not (void? instruction)))
         (when (not current-block-label)
           (set! current-block-label "entry"))
         (set! current-instructions (cons instruction current-instructions)))
       (finalize-block)]

      ;; For other instructions
      [else
       (define instruction (parse-llvm line))
       (when (and instruction (not (void? instruction)))
         (when (not current-block-label)
           (set! current-block-label "entry"))
         (set! current-instructions (cons instruction current-instructions)))]))

  ;; Finalize & Reverse the blocks
  (finalize-block)
  (reverse blocks))

;; Find a basic block by its label in a list of blocks
(define (find-block-by-label label blocks)
  (define found-block
    (for/first ([block blocks] #:when (equal? label (BasicBlock-label block)))
      block))  ;; Return the first matching block
  (if found-block
      (begin
        found-block)
      (error "Basic block with label ~a not found" label)))

;; Parse a function from its lines
(define (parse-function lines)
  ;; Check that lines are not empty
  (if (null? lines)
      (error "No lines to parse.")
      (let* ([function-name (car lines)]        ;; The first line contains the function definition
             [function-body (cdr lines)]        ;; The remaining lines contain the function body
             [basic-blocks (parse-basic-blocks function-body)]) ;; Parse the basic blocks from the body

        ;; Reorder basic blocks to correct order
        (define block-order '("entry" "lbl_t" "lbl_f" "end"))
        (define label-to-block (for/list ([block basic-blocks])
                                 (cons (BasicBlock-label block) block)))
        (define ordered-blocks
          (for/list ([label (in-list block-order)])
            (let ([block (assoc label label-to-block)])
              (if block
                  (cdr block)
                  (error "Block with label ~a not found in function ~a" label function-name)))))

        ;; Return a Function structure with the parsed name and ordered basic blocks
        (Function function-name ordered-blocks))))


