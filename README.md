# PetitCCompiler

Compiler for a small part of the C programming language that we'll call `PetitC`, made with the `OCaml` language.

---


## Dependencies

- `opam`
- `dune`
- `menhir`
- `ocamllex`
- `ocolor`
- `odoc` 

```sh
$ opam install ocolor odoc
```

---


## How to use the compiler

Check the [howto](HOWTO.md) file.

---


## Typing and Syntax checkers

### Ast
Nous avons choisi de décorer l'arbre de syntaxe abstraite au niveau des expressions, des déclarations d'instructions et des déclarations de fonctions afin d'avoir accès facilement aux localisations des erreurs. 
Nous avons ajouté un type `fct`. Ceci permet de remémorer qu'un identifiant d'une fonction lie aussi bien le type renvoyé par la fonction ainsi que la liste des types de chaque paramètre de la fonction.

- [ ] describe ast + choices made

### Lexer
Pour l'analyse lexicale, nous avons utilise l'outil `ocamllex`. Nous stockons les mots résérvés au langage sont enregistrés dans une table de hachage via une clé, qui est la chaîne de carctère correspondant au mot. Cette clé est sensible à la casse (une variable nommée `IF` ne sera pas reconnue en temps que mot clé, mais bien comme une variable). Nous gérons trois erreurs : les entiers illégaux, les caractères illégaux et les commentaires non finis. 

- [ ] explanations for the lexer rules (using ocamllex, implementation choices, etc...)
- [ ] possible lexing errors
- [ ] what can be added

### Parser

- [ ] explanations for the parser rules (using menhir, implementation choices, etc...)
- [ ] possible parser errors
- [ ] what can be added

### Typer

- [ ] describe typer + choices made

---


## Code production
