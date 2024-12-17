#lang racket

(require "ast.rkt"
         racket/match
         racket/string
         racket/cmdline)

(provide parse-llvm parse-function parse-basic-blocks find-block-by-label parse-llvm-file parse-call)


;; Check if a line is blank
(define (line-blank? line)
  (or (not line) (regexp-match? #px"^\\s*$" line)))


;; Parse LLVM instructions using regular expressions and match statements
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


              ;; Declare
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

              ;; Ret
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

              ;; icmp
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*icmp\\s+(\\w+)\\s+(\\w+)\\s+(%\\w+),\\s+(%\\w+|\\d+)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*icmp\\s+(\\w+)\\s+(\\w+)\\s+(%\\w+),\\s+(%\\w+|\\d+)$" line)]
                      [lhs (second m)]
                      [cond (third m)]
                      [type (fourth m)]
                      [op1 (fifth m)]
                      [op2 (sixth m)])
                 (LLVM-Instruction 'icmp (list lhs cond type op1 op2)))]

              ;; arithmetic: add, sub, mul, div
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*(add|sub|div|mul)\\s+(\\w+)\\s+(%\\w+|\\d+),\\s+(%\\w+|\\d+)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*(add|sub|div|mul)\\s+(\\w+)\\s+(%\\w+|\\d+),\\s+(%\\w+|\\d+)$" line)]
                      [lhs (second m)]
                      [opcode (third m)]
                      [type (fourth m)]
                      [op1 (fifth m)]
                      [op2 (sixth m)])
                 (LLVM-Instruction (string->symbol opcode) (list lhs type op1 op2)))]

              ;; phi
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*phi\\s+(.*)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*phi\\s+(.*)$" line)]
                      [lhs (second m)]
                      [ops (third m)])
                 (LLVM-Instruction 'phi (list lhs ops)))]

              ;; call with function and arguements
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

              ;; alloca
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*alloca\\s+(\\w+)(,\\s*align\\s+(\\d+))?\\s*$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*alloca\\s+(\\w+)(,\\s*align\\s+(\\d+))?\\s*$" line)]
                      [lhs (second m)]
                      [type (third m)]
                      [align-str (fifth m)]
                      [align (if align-str (string->number align-str) #f)])
                 (LLVM-Instruction 'alloca (list lhs type align)))]

              ;; getelementptr
              [(regexp-match? #px"^\\s*(%\\w+)\\s*=\\s*getelementptr\\s+(.*)$" line)
               (let* ([m (regexp-match #px"^\\s*(%\\w+)\\s*=\\s*getelementptr\\s+(.*)$" line)]
                      [lhs (second m)]
                      [operands (third m)])
                 (LLVM-Instruction 'getelementptr (list lhs operands)))]

              [else
               (error "Unknown instruction or format: ~a" line)])))))


;; Parse basic blocks from function body
(define (parse-basic-blocks lines)
  (define blocks '())
  (define current-block-label #f)
  (define current-instructions '())

  (define (finalize-block)
    (when (and current-block-label (not (null? current-instructions)))
      (set! blocks (cons (BasicBlock current-block-label (reverse current-instructions) '()) blocks)))
    (set! current-block-label #f)
    (set! current-instructions '()))

  (for ([line lines])
    (cond
      [(regexp-match #px"^(\\w+):$" line)
       (finalize-block)
       (set! current-instructions '())
       (set! current-block-label (second (regexp-match #px"^(\\w+):$" line)))]

      [(regexp-match? #px"^\\s*(br|ret)\\b" line)
       (define inst (parse-llvm line))
       (when inst
         (when (not current-block-label) (set! current-block-label "entry"))
         (set! current-instructions (cons inst current-instructions)))
       (finalize-block)]

      [else
       (define inst (parse-llvm line))
       (when inst
         (when (not current-block-label) (set! current-block-label "entry"))
         (set! current-instructions (cons inst current-instructions)))]))

  (finalize-block)
  (reverse blocks))


;; Helper function to parse call operands like "@callee(arg1, arg2, ...)"
(define (parse-call op-string)
  (define regex #px"@([\\w.]+)\\((.*)\\)"
  )
  (if (regexp-match regex op-string)
      (let ([match (regexp-match regex op-string)])
        (define callee (second match))
        (define raw-args (third match))
        (define args-list
          (if (line-blank? raw-args) '() (map string-trim (string-split raw-args #px","))))
        (values callee args-list))
      (values "unknown" '())))


(define (find-block-by-label label blocks)
  (for/first ([b blocks] #:when (equal? (BasicBlock-label b) label)) b))


;; Parse a single function from lines
(define (parse-function lines)
  (if (null? lines)
      (error "No lines to parse for function.")
      (let* ([header (car lines)]
             [body (cdr lines)]
             [basic-blocks (parse-basic-blocks body)]
             [f-name
              (match (parse-llvm header)
                [(LLVM-Instruction 'define (list ret-type fname params metadata)) fname]
                [_ (error "No function name in define")])])
        (Function f-name basic-blocks))))


;; Parse entire LLVM file
(define (parse-llvm-file lines)
  (define functions '())
  (define external-functions '())
  (define global-variables '())
  (define current-function-lines '())
  (define in-function #f)

  (for ([line lines])
    (cond
      [(regexp-match? #px"^\\s*define\\s+" line)
       (set! in-function #t)
       (set! current-function-lines (list line))]

      [(and in-function (regexp-match? #px"^\\s*\\}\\s*$" line))
       (set! current-function-lines (append current-function-lines (list line)))
       (define func (parse-function current-function-lines))
       (set! functions (cons func functions))
       (set! current-function-lines '())
       (set! in-function #f)]

      [in-function
       (set! current-function-lines (append current-function-lines (list line)))]

      [(regexp-match? #px"^\\s*declare\\s+" line)
       (define ext (parse-llvm line))
       (when (ExternalFunction? ext)
         (set! external-functions (cons ext external-functions)))]

      [(regexp-match? #px"^\\s*@\\w+\\s*=\\s*global\\s+" line)
       (define glob (parse-llvm line))
       (when (GlobalVariable? glob)
         (set! global-variables (cons glob global-variables)))]

      [else
       (void)]))

  (list (reverse functions) (reverse external-functions) (reverse global-variables)))
