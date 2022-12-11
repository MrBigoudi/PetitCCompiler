# COLORS
BLACK   := $(shell tput -Txterm setaf 0)
RED     := $(shell tput -Txterm setaf 1)
GREEN   := $(shell tput -Txterm setaf 2)
YELLOW  := $(shell tput -Txterm setaf 3)
BLUE    := $(shell tput -Txterm setaf 4)
MAGENTA := $(shell tput -Txterm setaf 5)
CYAN    := $(shell tput -Txterm setaf 6)
WHITE   := $(shell tput -Txterm setaf 7)
RESET   := $(shell tput -Txterm sgr0)

TARGET_MAX_CHAR_NUM=20

# FOLDERS
BIN_FOLDER    := compiler/bin
TEST_FOLDER   := compiler/tests
SYNTAX_FOLDER := $(BIN_FOLDER)/syntax

PARSER := $(SYNTAX_FOLDER)/parser.mly

# TARGETS
TARGET    := main.exe
COMPILER  := petitc
TEST      := test_cpy.sh
TEST_FLAGS := -v1

.PHONY: all help clean tests explain main


all: help


## Build the compiler
compiler: clean
	@printf "\n********** Building compiler **********\n"
	@cd compiler ; dune build
	@ln -s $(BIN_FOLDER)/$(TARGET) $(COMPILER)


## Build the compiler and run the tests : use TEST_FLAGS=-<option> to chose which test to run
tests: compiler
	@printf "\n********** Running tests **********\n"
	@./$(TEST_FOLDER)/$(TEST) $(TEST_FLAGS) $(COMPILER)


## Test if the parser generates conflicts
explain:
	@echo ""
	@echo "********** Checking parser conflicts **********"
	@echo ""
	menhir --base /tmp/parser --dump --explain $(PARSER)
	@cat /tmp/parser.conflicts


## Clean tests produced files
clean_tests:
	@printf "\n********** Cleaning tests **********\n"
ifneq ("$(wildcard $(TEST_FOLDER)/a.out)","")
	@rm $(TEST_FOLDER)/*.o
	@rm $(TEST_FOLDER)/*out*
endif


## Delete build files and executables
clean: clean_tests
	@printf "\n********** Cleaning project **********\n"
	@cd compiler ; dune clean
ifneq ("$(wildcard $(COMPILER))","")
	@rm $(COMPILER)
endif


## Show help
help:
	@echo ''
	@echo 'Usage:'
	@echo '  ${YELLOW}make${RESET} ${GREEN}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-_0-9]+:/ { \
			helpMessage = match(lastLine, /^## (.*)/); \
			if (helpMessage) { \
					helpCommand = substr($$1, 0, index($$1, ":")-1); \
					helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
					printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
			} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
	@echo ''
