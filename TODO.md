# Beforehead

- [ ] make odoc work


# Syntax 

- [ ] better parser errors


# Typing

- [x] maybe find another way to colour typing errors ?
- [ ] add warnings
- [x] store more infos in the typed_ast (the offset)
- [x] check the correctness of the offsets 
- [x] change the offset managment for parameters 
- [x] change the offset management for nested functions


# For the 12/11

- [x] check if comments are correct
- [ ] add mli files with function descriptions to make things clearer (if we have time)
- [x] add small part explaining that we've implemented the optional part + saving the offset during typing check 
- [x] make the archive without builds / todo / ...


# Code generation

- [x] complete all failwiths
- [ ] remove duplicate functions for dmap in ast_typed
- [x] modify mul and div for pointers?
- [ ] make the code better by removing duplicate things in code_producer.ml + add comments and better doc
- [x] add global counter for labels to avoid probles with nested instructions
- [x] add optional part (nested functions management)
- [x] correct optional part for pointers operations
- [ ] add a way to make redefinition of identifiers possible
- [ ] make a more optimized code generation


# Final due

- [x] improve the readme
- [x] change the howto
- [x] explain choices made (name of caller functions in -8(rbp) for nested functions)
- [x] make the tar
- [x] send the email
