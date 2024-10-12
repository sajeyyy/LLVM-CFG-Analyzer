#lang racket

(require "parser.rkt"
         "ast.rkt"
         racket/cmdline
         racket/match
         racket/string)

;; Build control-flow graph from all of the basic blocks
(define (build-cfg function)
  (printf "\nBuilding CFG for function: ~a\n" (Function-name function))
  
  (for ([block (Function-basic-blocks function)])
    (printf "Processing block: ~a\n" (BasicBlock-label block))

    (define instructions (BasicBlock-instructions block))
    (define successors '())

    ;; Check if the block has instructions to avoid errors with 'last'
    (if (null? instructions)
        (printf "Block ~a has no instructions; skipping setting successors.\n"
                (BasicBlock-label block))
        (let ([last-inst (last instructions)])
          (printf "Last instruction in block ~a:: ~a\n"
                  (BasicBlock-label block) last-inst)

          (match last-inst
            ;; If it's a branch instruction, get the labels
            [(LLVM-Instruction 'br operands)
             (printf "\nProcessing branch instruction with operands: ~a\n" operands)
             (cond
               ;; Conditional branch
               [(= (length operands) 4)
                (let* ([label-true (substring (list-ref operands 2) 1)] ;; Remove '%' from label
                       [label-false (substring (list-ref operands 3) 1)])
                  ;; Add true branch first, then false branch
                  (let ([succ-block-true (find-block-by-label label-true (Function-basic-blocks function))]
                        [succ-block-false (find-block-by-label label-false (Function-basic-blocks function))])
                    (when succ-block-true
                      (printf "Adding successor block (true branch): ~a\n" (BasicBlock-label succ-block-true))
                      (set! successors (append successors (list succ-block-true))))
                    (when succ-block-false
                      (printf "Adding successor block (false branch): ~a\n" (BasicBlock-label succ-block-false))
                      (set! successors (append successors (list succ-block-false))))))]
               ;; Unconditional branch
               [(= (length operands) 1)
                (let ([label (substring (first operands) 1)]) ;; Remove '%' from label
                  (let ([succ-block (find-block-by-label label (Function-basic-blocks function))])
                    (when succ-block
                      (printf "Adding successor block: ~a\n" (BasicBlock-label succ-block))
                      (set! successors (append successors (list succ-block))))))]
               ;; Else
               [else
                (printf "Unknown branch instruction format: ~a\n" operands)] )] 

            ;; Return instruction doesn't have successors
            [(LLVM-Instruction 'ret _)
             (printf "Return instruction: no successors for block ~a\n"
                     (BasicBlock-label block))
             (set! successors '())]

            ;; Default case for any other instruction
            [_ (printf "No successor for block ~a (other instruction)\n"
                       (BasicBlock-label block))])))
    ;; Set the successors for the block
    (printf "Setting successors for block ~a:: ~a\n"
            (BasicBlock-label block)
            (map BasicBlock-label successors))
    (set-BasicBlock-successors! block successors))
  function)

;; Generate Graphviz Dot Content from CFG
(define (generate-dot-content function)
  (printf "Generating dot content for function: ~a\n" (Function-name function))

  ;; Define the desired block order
  (define block-order '("entry" "lbl_t" "lbl_f" "end"))

  ;; Create a mapping from labels to blocks
  (define label-to-block
    (for/list ([block (Function-basic-blocks function)])
      (cons (BasicBlock-label block) block)))

  ;; Assign node IDs to each basic block according to block-order
  (define block-ids
    (for/list ([idx (in-naturals)]
               [label (in-list block-order)])
      (let ([block (assoc label label-to-block)])
        (if block
            (cons (cdr block) (format "Node~a" idx))
            (error "Block with label ~a not found in function ~a" label (Function-name function))))))

  ;; Create a mapping from blocks to node IDs
  (define block-to-node-id
    (make-hash block-ids))

  ;; Initialize dot-output
  (define dot-output "\ndigraph {\n")

  ;; For each basic block, generate the node and its edges in the desired order
  (for ([block-id block-ids])
    (define block (car block-id))
    (define node-id (cdr block-id))
    (unless (BasicBlock? block)
      (error "Invalid block detected in generate-dot-content"))

    ;; Generate the node with the block label
    (set! dot-output (string-append dot-output
                                    (format "    ~a [shape=record,label=\"~a\"]\n" node-id (BasicBlock-label block))))

    ;; Generate edges from this block
    (for ([succ (BasicBlock-successors block)] [i (in-naturals)])
      (define succ-node-id (hash-ref block-to-node-id succ))
      (define edge-label
        (if (and (equal? (BasicBlock-label block) "entry")
                 (= (length (BasicBlock-successors block)) 2))
            (number->string i)
            "0"))
      (set! dot-output (string-append dot-output
                                      (format "    ~a -> ~a [label=~a];\n" node-id succ-node-id edge-label)))))

  ;; Close the digraph
  (set! dot-output (string-append dot-output "}\n\n"))
  dot-output)

;; Output the CFG in Graphviz dot format to a file
(define (output-dot-file filename function)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out (generate-dot-content function)))
    #:exists 'replace))

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
