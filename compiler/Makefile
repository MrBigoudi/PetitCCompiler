# FOLDERS
BIN_FOLDER  := bin
TEST_FOLDER := test

# TARGETS
TARGET := main.exe
TEST   := compilerTests.exe


all: $(TARGET)

# Build tests
tests: 
	@echo -e "\n********** Building tests **********\n"
	dune build $(TEST_FOLDER)/$(TEST)

# Build main target
$(TARGET):
	@echo -e "\n********** Building compiler **********\n"
	dune build $(BIN_FOLDER)/$(TARGET)

# explain:
# 	menhir --base /tmp/parser --dump --explain parser.mly
# 	cat /tmp/parser.conflicts

clean:
	dune clean

.PHONY: all clean tests