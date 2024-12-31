EXECUTABLE = dataflow # Executable Name
MAIN_RKT = main.rkt # Main Racket file

# Files to include
SRC_FILES = main.rkt parser.rkt ast.rkt dataflow.rkt

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

