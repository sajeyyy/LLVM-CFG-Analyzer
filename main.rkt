#lang racket

(require "parser.rkt"
         "ast.rkt"
         racket/cmdline
         racket/match
         racket/string)

;; Main function to handle command-line input and process the LLVM file
(define (main)
  ;; Command-line argument parsing
  (define input-file
    (command-line
     #:program "LLVM CFG Analyzer"
     #:args (input)
     "Provide the LLVM IR input file."
     input))

  ;; Read the LLVM input file
  (define lines
    (with-input-from-file input-file
      (lambda ()
        (port->lines (current-input-port)))))

  ;; Parse the function(s) from the input file
  (define blocks (parse-function lines))

  ;; Build the CFG from parsed blocks
  (define cfg-blocks (build-cfg blocks))

  ;; Output the CFG to a DOT file
  (define output-file "main.dot")
  (output-dot-file output-file cfg-blocks)
  (printf "CFG has been output to '~a'.\n" output-file))

;; Run the main function
(main)
