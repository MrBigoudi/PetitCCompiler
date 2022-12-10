# petitc - Howto

The compiler comes with a `Makefile` to make building and testing easier.

---


## How to build the compiler

### Build the executable

To build the compiler you can type the command in the `compiler` directory
```sh
$ cd compiler
$ make compiler
```

This will build a `main.exe` in the `bin` directory. However, to make things more usable, a link `petitc` to this executable is created in the `compiler` directory. To see how to use the so build compiler check [this section](#How-to-use-the-compiler).


### Run automated tests

The compiler comes with a bunch of tests to check its features. These tests are run automatically using the script `compiler/tests/test_cpy.sh`. To run the tests you can just type:
```sh
$ make tests
```
This will run the tests for the `syntax correctness` in the verbose mode.

You can check independently the `syntax correctness`, the `typing correctness` and the `code production` with or without the verbose mode by using different options:

| &lt;option&gt;  | description |
| --------------: | ----------: |
|1                | To test the syntax correctness |
|2                | To test the typing correctness |
|3                | To test the code production |
|v1               | To test the syntax correctness in verbose mode |
|v2               | To test the typing correctness in verbose mode |
|v3               | To test the code production in verbose mode |
|1b               | To test the syntax correctness with nested functions |
|2b               | To test the typing correctness with nested functions |
|3b               | To test the code production with nested functions |
|v1b              | To test the syntax correctness in verbose mode with nested functions |
|v2b              | To test the typing correctness in verbose mode with nested functions |
|v3b              | To test the code production in verbose mode with nested functions |
|gcc              | To run gcc on the samples |
|all              | To test the syntax correctness, the typing correctness and the code production |
|vall             | To test the syntax correctness, the typing correctness and the code production in verbose mode |
|allb             | To test the syntax correctness, the typing correctness and the code production with nested functions |
|vallb            | To test the syntax correctness, the typing correctness and the code production in verbose mode with nested functions |
|part1            | To test the syntax and typing correctness with nested functions |

To run the tests as you want, you can use the command:
```sh
$ make tests TEST_FLAGS=-<option>
```

---


## How to use the compiler

To use the compiler, just give it a valid `PetitC` file as input and it will generate an `.as` file corresponding to the assembly code generated.

```sh
$ ./petitc <inputFile>.c
```

You can use several options for the compiler:

| &lt;option&gt;  | description |
| --------------: | ----------: |
| --parse-only    | The compiler will stop after parsing the input file |
| --type-only     | The compiler will stop after parsing the input file and checking it semantically |

```sh
$ ./petitc <option> <inputFile>.c
```