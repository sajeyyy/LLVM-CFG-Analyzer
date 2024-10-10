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
        (printf "\nProcessing line: ~a\n" line) ;; Log the line being processed

        ;; Ignore lines with just curly braces
        (if (regexp-match? #px"^\\s*[{}]\\s*$" line)
            (begin
              (printf "Ignoring curly brace: ~a\n" line)  ;; Just log and skip curly brace
              #f)  ;; Return #f for curly braces

            ;; Else, proceed to process the line
            ;; First check for each pattern using `regexp-match?`
            (cond
              ;; Match 'alloca' (e.g., "%ptr = alloca i32")
              [(regexp-match? #px"^\\s*(%\\w+) = alloca (\\w+)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = alloca (\\w+)$" line)]
                      [lhs (second matches)]
                      [type (third matches)])
                 (LLVM-Instruction 'alloca (list lhs type)))]

              ;; Match 'getelementpointer' (e.g., "%gep = getelementptr i32, i32* %ptr, i32 0")
              [(regexp-match? #px"^\\s*(%\\w+) = getelementptr (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = getelementptr (.*)$" line)]
                      [lhs (second matches)]
                      [operands (third matches)])
                 (LLVM-Instruction 'getelementpointer (list lhs operands)))]

              ;; Match 'load' (e.g., "%val = load i32, i32* %ptr")
              [(regexp-match? #px"^\\s*(%\\w+) = load (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = load (.*)$" line)]
                      [lhs (second matches)]
                      [operands (third matches)])
                 (LLVM-Instruction 'load (list lhs operands)))]

              ;; Match 'store' (e.g., "store i32 %val, i32* %ptr")
              [(regexp-match? #px"^\\s*store (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*store (.*)$" line)]
                      [operands (second matches)])
                 (LLVM-Instruction 'store (list operands)))]

              ;; Match 'ret' (e.g., "ret i32 %val")
              [(regexp-match? #px"^\\s*ret (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*ret (.*)$" line)]
                      [operands (second matches)])
                 (LLVM-Instruction 'ret (list operands)))]

              ;; Match 'icmp' (e.g., "%cmp = icmp eq i32 %argc, 1")
              [(regexp-match? #px"^\\s*(%\\w+) = icmp (\\w+) (\\w+) (%\\w+), (%\\w+|\\d+)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = icmp (\\w+) (\\w+) (%\\w+), (%\\w+|\\d+)$" line)]
                      [lhs (second matches)]
                      [cond (third matches)]
                      [type (fourth matches)]
                      [op1 (fifth matches)]
                      [op2 (sixth matches)])
                 (LLVM-Instruction 'icmp (list lhs cond type op1 op2)))]

              ;; Match conditional 'br' instruction
              [(regexp-match? #px"^\\s*br\\s+(i\\d+)\\s+(%\\w+),\\s+label\\s+(%\\w+),\\s+label\\s+(%\\w+)$" line)
               (let* ([matches (regexp-match #px"^\\s*br\\s+(i\\d+)\\s+(%\\w+),\\s+label\\s+(%\\w+),\\s+label\\s+(%\\w+)$" line)]
                      [cond-type (second matches)]
                      [cond-var (third matches)]
                      [label-true (fourth matches)]
                      [label-false (fifth matches)])
                 (LLVM-Instruction 'br (list cond-type cond-var label-true label-false)))]

              ;; Match unconditional 'br' instruction
              [(regexp-match? #px"^\\s*br\\s+label\\s+(%\\w+)$" line)
               (let* ([matches (regexp-match #px"^\\s*br\\s+label\\s+(%\\w+)$" line)]
                      [label (second matches)])
                 (LLVM-Instruction 'br (list label)))]

              ;; Match 'sub', 'div', 'mul', 'add'
              [(regexp-match? #px"^\\s*(%\\w+) = (add|sub|div|mul) (\\w+) (%\\w+|\\d+), (%\\w+|\\d+)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = (add|sub|div|mul) (\\w+) (%\\w+|\\d+), (%\\w+|\\d+)$" line)]
                      [lhs (second matches)]
                      [opcode (third matches)]
                      [type (fourth matches)]
                      [op1 (fifth matches)]
                      [op2 (sixth matches)])
                 (LLVM-Instruction (string->symbol opcode) (list lhs type op1 op2)))]

              ;; Match 'phi' (e.g., "%res = phi i32 [%val1, %then], [%val2, %else]")
              [(regexp-match? #px"^\\s*(%\\w+) = phi (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = phi (.*)$" line)]
                      [lhs (second matches)]
                      [operands (third matches)])
                 (LLVM-Instruction 'phi (list lhs operands)))]

              ;; Match 'call' (e.g., "%result = call i32 @func(i32 %a)")
              [(regexp-match? #px"^\\s*(%\\w+) = call (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*(%\\w+) = call (.*)$" line)]
                      [lhs (second matches)]
                      [operands (third matches)])
                 (LLVM-Instruction 'call (list lhs operands)))]

              ;; Match 'define' (e.g., "define i32 @main(i32 %argc)")
              [(regexp-match? #px"^\\s*define (.*)$" line)
               (let* ([matches (regexp-match #px"^\\s*define (.*)$" line)]
                      [signature (second matches)])
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
  (when (and current-block-label (not (null? current-instructions)))
    (printf "Finalizing block: ~a with instructions: ~a\n" current-block-label current-instructions)
    (set! blocks (cons (BasicBlock current-block-label (reverse current-instructions) '()) blocks)))
  ;; Reset the state variables outside the 'when' block
  (set! current-block-label #f)
  (set! current-instructions '()))

  ;; Process each line
(for ([line lines])
  (printf "Processing line in basic block: ~a\n" line)
  (cond
    ;; If the line is a label (start of a block)
    [(regexp-match #px"^(\\w+):$" line)
     (finalize-block) ;; Finalize the previous block
     (set! current-instructions '()) ;; Reset instructions for the new block
     (set! current-block-label (second (regexp-match #px"^(\\w+):$" line)))
     (printf "New block detected: ~a\n" current-block-label)]

    ;; If it's a control flow instruction (end of a block)
    [(regexp-match #px"^\\s*(br|ret)\\b" line)
     (define instruction (parse-llvm line))
     (printf "Parsed control flow instruction: ~a\n" instruction)
     (when (and instruction (not (void? instruction)))
       (when (not current-block-label)
         (set! current-block-label "entry"))
       (set! current-instructions (cons instruction current-instructions)))
     (finalize-block)]

    ;; For other instructions
    [else
     (define instruction (parse-llvm line))
     (printf "Parsed instruction: ~a\n" instruction)
     (when (and instruction (not (void? instruction)))
       (when (not current-block-label)
         (set! current-block-label "entry"))
       (set! current-instructions (cons instruction current-instructions)))]))

  (printf "Parsed basic blocks: ~a\n" (map BasicBlock-label blocks))


;; Finalize the last block
(printf "Finalizing last block if any.\n")
(finalize-block)

;; Print the blocks before reversing
(printf "Blocks before reverse: ~a\n" blocks)

(printf "Parsed basic blocks: ~a\n" (map BasicBlock-label (reverse blocks)))
(reverse blocks))


;; Find a basic block by its label in a list of blocks
(define (find-block-by-label label blocks)
  (define found-block
    (for/first ([block blocks] #:when (equal? label (BasicBlock-label block)))
      block))  ;; Return the first matching block
  (if found-block
      (begin
        (printf "Found block for label ~a: ~a\n" label (BasicBlock-label found-block))
        found-block)
      (error "Basic block with label ~a not found" label)))

;; Parse a single function from its lines
(define (parse-function lines)
  ;; Check that lines are not empty to avoid errors
  (if (null? lines)
      (error "No lines to parse.")
      (let* ([function-name (car lines)]        ;; The first line contains the function definition
             [function-body (cdr lines)]        ;; The remaining lines contain the function body
             [basic-blocks (parse-basic-blocks function-body)]) ;; Parse the basic blocks from the body

        ;; Debug print statement to verify basic blocks
        (printf "Parsed basic blocks: ~a\n" (map BasicBlock-label basic-blocks))

        ;; Return a Function struct with the parsed name and basic blocks
        (Function function-name basic-blocks))))

;; Convert LLVM-Instruction to string representation for output
(define (LLVM-Instruction->string inst)
  (begin
    ;; Debug print to show the structure of the instruction being processed
    (printf "Processing LLVM-Instruction: ~a\n" inst)

    ;; Match the instruction type
    (match inst

       [#f (printf "Found void instruction!\n")""]

      ;; Match 'alloca'
      [(LLVM-Instruction 'alloca `(,lhs ,type))
       (format "~a = alloca ~a" lhs type)]

      ;; Match 'getelementpointer'
      [(LLVM-Instruction 'getelementpointer `(,lhs ,operands))
       (format "~a = getelementptr ~a" lhs operands)]

      ;; Match 'load'
      [(LLVM-Instruction 'load `(,lhs ,operands))
       (format "~a = load ~a" lhs operands)]

      ;; Match 'store'
      [(LLVM-Instruction 'store `(,operands))
       (format "store ~a" operands)]

      ;; Match 'ret'
      [(LLVM-Instruction 'ret `(,operands))
       (format "ret ~a" operands)]

      ;; Match 'icmp'
      [(LLVM-Instruction 'icmp `(,lhs ,cond ,type ,op1 ,op2))
       (format "~a = icmp ~a ~a, ~a, ~a" lhs cond type op1 op2)]

      ;; Match 'br'
      [(LLVM-Instruction 'br operands)
       (format "br ~a" (string-join operands ", "))]

      ;; Match 'add', 'sub', 'div', 'mul' with string operands
      [(LLVM-Instruction (or 'add 'sub 'div 'mul) `(,lhs ,type ,op1 ,op2))
       (format "~a = ~a ~a, ~a, ~a" lhs (symbol->string (LLVM-Instruction-opcode inst)) type op1 op2)]

      ;; Match 'phi'
      [(LLVM-Instruction 'phi `(,lhs ,operands))
       (format "~a = phi ~a" lhs operands)]

      ;; Match 'call'
      [(LLVM-Instruction 'call `(,lhs ,operands))
       (format "~a = call ~a" lhs operands)]

      ;; Match 'define'
      [(LLVM-Instruction 'define `(,signature))
       (format "define ~a" signature)]

      ;; Handle unknown instruction types
      [_ (begin
           (printf "\nError: Unknown LLVM instruction type encountered: ~a\n" inst)
           (error "Unknown LLVM instruction type: ~a" inst))])))

;; Build control-flow graph (CFG) from basic blocks
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
        (let ([last-inst (last instructions)]) ;; Use let instead of define
          (printf "Last instruction in block ~a:: ~a\n"
                  (BasicBlock-label block) last-inst)

          (match last-inst
            ;; If it's a branch instruction, extract the labels
            [(LLVM-Instruction 'br operands)
             (printf "\nProcessing branch instruction with operands: ~a\n" operands)
             (cond
               ;; Conditional branch
               [(= (length operands) 4)
                (let* ([label-true (substring (list-ref operands 2) 1)] ;; Remove '%' from label
                       [label-false (substring (list-ref operands 3) 1)])
                  (for ([label (list label-true label-false)])
                    (let ([succ-block (find-block-by-label label (Function-basic-blocks function))])
                      (when succ-block
                        (printf "Adding successor block: ~a\n" (BasicBlock-label succ-block))
                        (set! successors (cons succ-block successors))))))]
               ;; Unconditional branch
               [(= (length operands) 1)
                (let ([label (substring (first operands) 1)]) ;; Remove '%' from label
                  (let ([succ-block (find-block-by-label label (Function-basic-blocks function))])
                    (when succ-block
                      (printf "Adding successor block: ~a\n" (BasicBlock-label succ-block))
                      (set! successors (cons succ-block successors)))))]
               ;; Else
               [else
                (printf "Unknown branch instruction format: ~a\n" operands)]
               ) ;; End of cond
             ] ;; End of match case for 'br'

            ;; Return instruction doesn't have successors
            [(LLVM-Instruction 'ret _)
             (printf "Return instruction: no successors for block ~a\n"
                     (BasicBlock-label block))
             (set! successors '())]

            ;; Default case for any other instruction
            [_ (printf "No successor for block ~a (other instruction)\n"
                       (BasicBlock-label block))]
            ) ;; End of match
          )) ;; End of let and if

    ;; Set the successors for the block
    (printf "Setting successors for block ~a:: ~a\n"
            (BasicBlock-label block)
            (map BasicBlock-label successors))
    (set-BasicBlock-successors! block successors)
    ) ;; End of for loop body

  function) ;; Return the function with updated CFG
 ;; Return the function with updated CFG



;; Generate Graphviz dot content from CFG
(define (generate-dot-content function)
  (printf "Generating dot content for function: ~a\n" (Function-name function))

  ;; Print the basic blocks
  (printf "Function's basic blocks: ~a\n" (Function-basic-blocks function))

  (define dot-output "digraph {\n")

  ;; Assign node IDs to each basic block
  (define block-ids
    (for/list ([(block idx) (in-indexed (Function-basic-blocks function))])
      (cons block (format "Node~a" idx))))

  ;; Debug: Print block-ids
  (printf "block-ids: ~a\n" block-ids)

  ;; For each basic block, generate a node
  (for ([block-id block-ids])
    (define block (car block-id))
    (define node-id (cdr block-id))
    (printf "Block-id: ~a\n" block-id)
    (printf "Block: ~a\n" block)
    (unless (BasicBlock? block)
      (printf "Error: block is not a BasicBlock: ~a\n" block)
      (error "Invalid block detected in generate-dot-content"))
    (printf "Generating node for block: ~a with node-id: ~a\n" (BasicBlock-label block) node-id)

    ;; Generate the node
    (set! dot-output (string-append dot-output (format "    ~a [shape=record,label=\"~a\"]\n" node-id (BasicBlock-label block)))))

  ;; Generate the edges based on successors
  (for ([block-id block-ids])
    (define block (car block-id))
    (define node-id (cdr block-id))
    (printf "Processing block ~a for successors\n" node-id)

    ;; Add edges to successor blocks
    (for ([succ (BasicBlock-successors block)] [i (in-naturals)])
      (define succ-id (assoc succ block-ids))
      (if succ-id
          (begin
            (printf "Adding edge from ~a to ~a\n" node-id (cdr succ-id))
            (set! dot-output (string-append dot-output
                                            (format "    ~a -> ~a [label=~a];\n" node-id (cdr succ-id) i))))
          (printf "No successor block found for block ~a\n" node-id))))

  ;; Close the digraph
  (set! dot-output (string-append dot-output "}\n"))
  dot-output)


;; Output the CFG in Graphviz dot format to a file
(define (output-dot-file filename function)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out (generate-dot-content function)))
    #:exists 'replace))
