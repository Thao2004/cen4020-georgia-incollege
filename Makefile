# Name of the program
BIN=bin/InCollege
SRC=src/InCollege.cob

# Default target
all: run

# Compile the COBOL program into bin/
build:
	@cobc -x -free -o $(BIN) $(SRC)

# Run after building
run: build
	@./$(BIN)

# Clean generated files
clean:
	rm -f $(BIN) InCollege-Output.txt accounts.dat
