# Beforehead

- ajouter odoc

# Syntax 

- [x] analyse lexicale
- [x] analyse syntaxique
- [ ] refaire l'ast : problèmes avec la reconnaissance des types
- [ ] affichage des erreurs (commencé avec le parser, mieux décorer les arbres)

- ~~incr decr neg -> sucre~~ : En fait non, il faut démultiplier les unary op pour le typage *ET* la production de code.

# Typage
- [ ] Créer un nouvel arbre de syntaxe abstraite _typé_ afin de garder pour chaque noeud une information sur le type. 
- [ ] Moins se focus sur les warnings de GCC (trop complexe) -> faire la règle giga maxi simple du sujet
- [ ] Faire des fonctions d'assert pour que : types soient equivalents et si expr est une lvalue
- [ ] Le typage doit renvoyer un AST typé (arbre décoré des types en plus des noeuds de l'AST originale) -> on stcoke dans l'environnement pas que les types mais aussi d'autres infos.
- [ ] Les expressions ne modifient pas l'environnement
- [ ] Gérer les différents environnements pour les différents blocs 
- [ ] Les messages d'erreur doivent être modifiés en fonction du type rencontré : pas juste un failwith !

# Generation de code
