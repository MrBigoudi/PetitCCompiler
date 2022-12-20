# Beforehead

- [ ] make odoc work


# Syntax 

- [ ] better parser errors


# Typage

- [ ] maybe find another way to colour typing errors ?
- [ ] add warnings
- [x] store more infos in the typed_ast (the offset)
- [ ] check the correctness of the offsets 
- [x] change the offset managment for parameters 
- [ ] cheng the offset management for nested functions


# For the 12/11

- [ ] check if comments are correct
- [ ] add mli files with function descriptions to make things clearer (if we have time)
- [ ] add small part explaining that we've implemented the optional part + saving the offset during typing check 
- [ ] make the archive without builds / todo / ...


# Generation de code

- [ ] complete all failwiths
- [ ] remove duplicate functions for dmap in ast_typed
- [ ] modify mul and div for pointers ?
- [ ] make the code by removing duplicate things in code_producer.ml + add comments and better doc
- [ ] add global counter for labels to avoid probles with nested instructions