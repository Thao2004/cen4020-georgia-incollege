# Name of the program
BIN=bin/InCollege
SRC=src/InCollege.cob

# Default target
all: run

# Ensure bin directory exists
$(BIN): $(SRC)
	@mkdir -p $(dir $(BIN))
	@cobc -x -free -o $(BIN) $(SRC)

# Compile the COBOL program into bin/
build: $(BIN)

# Run after building
run: build
	@./$(BIN)

# Clean generated files
clean:
	rm -f $(BIN) InCollege-Output.txt accounts.dat profiles.dat connections.dat requests.dat job-postings.dat applications.dat
