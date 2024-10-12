# Makefile for compiling the Racket program into an executable

# Name of the executable
EXECUTABLE = graph

# Main Racket file
MAIN_RKT = main.rkt

# Files to include in the compilation
SRC_FILES = main.rkt parser.rkt ast.rkt

.PHONY: all clean

all: $(EXECUTABLE)

$(EXECUTABLE): $(SRC_FILES)
	@echo "Compiling Racket program into executable..."
	raco exe -o $(EXECUTABLE) $(MAIN_RKT)

clean:
	@echo "Cleaning up..."
	rm -f $(EXECUTABLE)
	rm -f $(EXECUTABLE).dll $(EXECUTABLE).so
	rm -f main.dot main.png

