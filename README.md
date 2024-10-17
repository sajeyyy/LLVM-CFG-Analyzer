# LLVM IR Control-Flow Graph Analyzer

This project is an LLVM Intermediate Representation (IR) Control-Flow Graph (CFG) analyzer built using Racket. The tool parses LLVM IR code, extracts control-flow information, and generates a control-flow graph in Graphviz DOT format to visualize the relationships between basic blocks.


## Features

- **LLVM IR Parsing**: Parses LLVM IR code to understand control-flow structures.
- **CFG Generation**: Analyzes the control-flow and generates a control-flow graph.
- **Graphviz Integration**: Exports the CFG in Graphviz DOT format for visualization.
- **Basic Block Detection**: Identifies and outputs basic blocks and their successors within the LLVM IR.


## Prerequisites

- [Racket](https://racket-lang.org/) (version 8.0 or later)
- [Graphviz](https://graphviz.org/) to visualize the generated DOT files


### Installation

1. **Install Racket**: Download and install Racket from [here](https://racket-lang.org/download/).
   
2. **Install Graphviz**:
   - **macOS**: Install with Homebrew:
     ```
     brew install graphviz
     ```
   - **Ubuntu**: Install with APT:
     ```
     sudo apt install graphviz
     ```
   - **Windows**: Download the installer from the [Graphviz download page](https://graphviz.org/download/).


## Usage

- **Prepare an LLVM IR file:**
  - Create an LLVM IR file, for example `test.ll`:

```
  define i32 @main(i32 %argc) {
    %noArgs = icmp eq i32 %argc, 1
    br i1 %noArgs, label %lbl_t, label %lbl_f
  lbl_t:
    %varT = add i32 1, 0
    br label %end
  lbl_f:
    %varF = add i32 2, 0
    br label %end
  end:
    %var = phi i32 [%varT, %lbl_t], [%varF, %lbl_f]
    ret i32 %var
  }
```
- **Run the program:**

  - Use the `Make` command the create an excutable

  - Run the newly created executable named `graph` with the LLVM file as the arguement:
    `./graph test.ll`
  
- **Test.ll Example Output**
    ```cat main.dot
      digraph {
        Node0 [shape=record,label="entry"]
        Node0 -> Node1 [label=0];
        Node0 -> Node2 [label=1];
        Node1 [shape=record,label="lbl_t"]
        Node1 -> Node3 [label=0];
        Node2 [shape=record,label="lbl_f"]
        Node2 -> Node3 [label=0];
        Node3 [shape=record,label="end"]
        }
    ```
  
  - Visual the Control Flow Graph (Graphviz will convert the output dotfile to a PNG)
    `dot -Tpng main.dot -o cfg.png`

.
├── ast.rkt               # Defines the AST node structures for LLVM instructions, basic blocks, and functions
├── main.rkt              # Main entry point for the program and constructs the CFGs
├── parser.rkt            # Contains the logic for parsing LLVM IR
├── test.ll               # Example LLVM IR file for testing
└── README.md             # Documentation

