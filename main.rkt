#lang racket

(require "parser.rkt"
         "ast.rkt"
         "dataflow.rkt"
         racket/cmdline
         racket/match
         racket/string)

;;-------------------------------------------
;; 1) Command-Line
;;-------------------------------------------
(define mode "cfg")    ; Default if user doesn't specify --mode
(define input-file #f) ; We'll store the LLVM IR file path

(command-line
 #:program "LLVM-Analyzer"

 ;; Let user specify once: --mode <cfg|dataflow|both>
 #:once-each
 [("--mode") m
  "Specify which mode to run: cfg, dataflow, or both"
  (set! mode m)]

 ;; Then a single positional argument for the IR file
 #:args (file)
 "LLVM IR input file to analyze"
 (set! input-file file))

;;-------------------------------------------
;; 2) CFG-Building (unchanged from your logic)
;;-------------------------------------------
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
              (when succ-block-true
                (set! successors (cons succ-block-true successors)))
              (when succ-block-false
                (set! successors (cons succ-block-false successors)))]
             [(= (length operands) 1)
              (define label (substring (first operands) 1))
              (define succ-block
                (find-block-by-label label (Function-basic-blocks function)))
              (when succ-block
                (set! successors (cons succ-block successors)))]
             [else (void)])]
          [(LLVM-Instruction 'ret _)
           (set! successors '())]
          [_ (void)])))
    (set-BasicBlock-successors! block (reverse successors)))
  function)

;;-------------------------------------------
;; 3) Dot Generation
;;   (We create a small helper that outputs the CFG as .dot)
;;-------------------------------------------
(define (output-cfg-dot function filename)
  (define blocks (Function-basic-blocks function))
  (define dot "digraph {\n")

  ;; We'll label each block Node0, Node1, etc.
  (for ([blk blocks] [i (in-naturals)])
    (define node-id (format "Node~a" i))
    (define lbl (BasicBlock-label blk))
    (set! dot
          (string-append dot
                         (format "  ~a [label=\"~a\"];\n" node-id lbl))))

  ;; Edges
  (for ([blk blocks] [i (in-naturals)])
    (define node-id (format "Node~a" i))
    (for ([succ (BasicBlock-successors blk)])
      (define j (index-of succ blocks))
      (when j
        (define succ-id (format "Node~a" j))
        (set! dot
              (string-append dot
                             (format "  ~a -> ~a;\n" node-id succ-id))))))

  (set! dot (string-append dot "}\n"))

  ;; Write to file
  (call-with-output-file filename
    (lambda (out) (fprintf out "~a" dot))
    #:exists 'replace))

;; Helper for indexing
(define (index-of x lst [i 0])
  (cond
    [(null? lst) #f]
    [(eq? x (car lst)) i]
    [else (index-of x (cdr lst) (add1 i))]))

;;-------------------------------------------
;; 4) MAIN
;;-------------------------------------------
(define (main)
  ;; 1) Parse entire file into (list-of functions, externals, globals)
  (define lines
    (with-input-from-file input-file
      (lambda () (port->lines (current-input-port)))))
  (define parsed (parse-llvm-file lines))
  (define functions (first parsed))
  (define externals (second parsed)) ; unused here
  (define globals  (third parsed))   ; unused here

  ;; 2) For each function, build CFG
  (for ([func functions])
    (define cfg-func (build-cfg func))

    ;; If user wants CFG or BOTH, output a .dot for the function
    (when (or (string=? mode "cfg")
              (string=? mode "both"))
      (define dotfile (string-append (Function-name func) ".dot"))
      (output-cfg-dot cfg-func dotfile)
      (printf "Wrote CFG to '~a' for function '~a'.\n"
              dotfile (Function-name func)))

    ;; If user wants dataflow or BOTH, do dataflow, print FLOW or NO FLOW
    (when (or (string=? mode "dataflow")
              (string=? mode "both"))
      (define flow? (dataflow-analyze cfg-func))
      (if flow?
          (printf "\nFLOW\n\n")
          (printf "\nNO FLOW\n\n")))))

(main)
