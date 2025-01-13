#lang racket

(require "parser.rkt"
         "ast.rkt"
         "dataflow.rkt"
         racket/cmdline
         racket/match
         racket/string)

(define mode "cfg")
(define input-file #f)

;; -------------------------------
;; Command-line Parsing
;; -------------------------------

(command-line
 #:program "LLVM-Analyzer"

 #:once-each
   [("--mode") m
        "Set Mode" (set! mode m)]

 ;; Finish clause to parse last argument
 #:args (filename)
 "LLVM IR input file to analyze."
 (set! input-file filename))

;; ---------------------------------------
;; Build CFG
;; ---------------------------------------
(define (build-cfg function)
  (for ([block (Function-basic-blocks function)])
    (define instructions (BasicBlock-instructions block))
    (define successors '())
    (unless (null? instructions)
      (let ([last-inst (last instructions)])
        (match last-inst
          [(LLVM-Instruction 'br operands)
           (cond
             [(= (length operands) 4)
              (define label-true (substring (list-ref operands 2) 1))
              (define label-false (substring (list-ref operands 3) 1))
              (define succ-block-true
                (find-block-by-label label-true (Function-basic-blocks function)))
              (define succ-block-false
                (find-block-by-label label-false (Function-basic-blocks function)))
              (when succ-block-true (set! successors (cons succ-block-true successors)))
              (when succ-block-false (set! successors (cons succ-block-false successors)))]
             [(= (length operands) 1)
              (define label (substring (first operands) 1))
              (define succ-block
                (find-block-by-label label (Function-basic-blocks function)))
              (when succ-block (set! successors (cons succ-block successors)))]
             [else (void)])]
          [(LLVM-Instruction 'ret _)
           (set! successors '())]
          [_ (void)])))
    (set-BasicBlock-successors! block (reverse successors)))
  function)

;; ---------------------------------------
;; Generate DOT
;; ---------------------------------------
(define (generate-dot-content function [block-in (λ(_) '())] [block-out (λ(_) '())])
  (define blocks (Function-basic-blocks function))
  (define dot-output "digraph {\n")

  (for ([blk blocks] [i (in-naturals)])
    (define node-id (format "Node~a" i))
    (define lbl (BasicBlock-label blk))

    ;; Possibly embed dataflow sets if mode is "dataflow" or "both"
    (define in-str (string-join (block-in blk) ","))
    (define out-str (string-join (block-out blk) ","))
    (define node-label
      (cond
        [(or (string=? mode "dataflow") (string=? mode "both"))
         (format "~a\\nin: {~a}\\nout: {~a}" lbl in-str out-str)]
        [else
         (format "~a" lbl)]))

    (set! dot-output
          (string-append dot-output
                         (format "  ~a [label=\"~a\"];\n" node-id node-label))))

  ;; Edges
  (for ([blk blocks] [i (in-naturals)])
    (define node-id (format "Node~a" i))
    (for ([succ (BasicBlock-successors blk)])
      (define j (index-of succ blocks))
      (when j
        (define succ-id (format "Node~a" j))
        (set! dot-output
              (string-append dot-output
                             (format "  ~a -> ~a;\n" node-id succ-id))))))

  (string-append dot-output "}\n"))

;; Helper to find a block's index in a list
(define (index-of x lst [i 0])
  (cond
    [(null? lst) #f]
    [(eq? x (car lst)) i]
    [else (index-of x (cdr lst) (add1 i))]))

;; ---------------------------------------
;; Main Flow
;; ---------------------------------------
(define (main)

  ;; 1) Parse the input file
  (define lines
    (with-input-from-file input-file
      (lambda () (port->lines (current-input-port)))))

  ;; 2) Parse a single function
  (define the-func (parse-function lines))

  ;; 3) Build CFG
  (define cfg-func (build-cfg the-func))

  ;; 4) Possibly do dataflow
  (define block-in (λ(_)'()))
  (define block-out (λ(_)'()))
  (when (or (string=? mode "dataflow")
            (string=? mode "both"))
    (define-values (in-sets out-sets) (dataflow-analyze cfg-func))
    (set! block-in in-sets)
    (set! block-out out-sets))

  ;; 5) Generate .dot
  (define dot-str (generate-dot-content cfg-func block-in block-out))
  (call-with-output-file "main.dot"
    (lambda (out)
      (fprintf out "~a" dot-str))
    #:exists 'replace)
  (printf "Wrote 'main.dot' for mode '~a'.\n" mode))

(main)
