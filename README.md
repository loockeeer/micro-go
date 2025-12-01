# Mini-Go Compiler

Squelette du cours utilisé

## Ajout automatique des points-virgules

Pour faire cela, j'ai ajouté une référence dans le lexer :
```ocaml
  let candidate_for_semi = ref false;;
  let ms () = candidate_for_semi := true;;
  let nms () = candidate_for_semi := false;;
```

`candidate_for_semi` à `true` signifie que le dernier lexème est compatible avec
l'ajoute d'un point-virgule à sa suite.
Les fonctions `ms` et `nms` me permettent juste d'écrire plus rapidement ce
qu'elles font.

Dès lors qu'une règle du lexer renvoie un des caractères permettant l'ajout automatique
d'un point-virgule dans le sujet, j'appelle `ms()` avant de passer au lexème suivant.
Dans le cas contraire, j'appelle `nms()`. Ensuite, lorsque l'on rencontre un `\n`,
en fonction de la valeur de `candidate_for_semi` on ajoute ou non un `SEMI` avant de
continuer le lexing.

## Ajout d'un terminal «NEW»

Pour gérer correctement l'appel `new(s)`, j'ai ajouté un symbole terminal de 
mot-clé NEW (pour "new"), et une règle dans le parser.
C'est très similaire à ce qui est fait pour le print

## Ajout de messages d'erreurs sur les fautes de frappes

La distance de Levenshtein peut être utilisée pour détecter les fautes de frappes.
J'ai donc codé sa version récursive et ai augmenté les messages d'erreurs de
fonction/variable/structure/membre inconnu(e)(s) comme ceci :

**Avant** :
```
File "tests/var.go", line 15, characters 4-17:
error: undefined: yy.
```

**Après** :
```
File "tests/var.go", line 15, characters 4-17:
error: undefined: yy. Did you mean y ?
```

Les fonctions qui permettent cela sont en haut de `typechecker.ml`

## Ajout d'un argument pour la visualisation des AST

J'ai fait quelques mineurs ajouts à `mgoast.ml`, à `mgoc.ml`, ainsi qu'à `dune` pour pouvoir
afficher les AST grâce à des fonctions d'affichage générées par `ppx_deriving`.
On peut utilier cette fonctionnalité en ajoutant l'option `--verbose` en ligne
de commande. Cela affichera l'AST avant de faire le typecheck.

## Modification de l'AST pour rectifier une erreur

Dans mgoast.ml tel que fourni, on a :
```ocaml
and instr_desc =
  (* Écriture dans une variable ou un attribut *)
  | Set of expr list * expr list
  | Inc of expr
  | Dec of expr
  (* Structures de contrôle usuelles *)
  | If of expr * seq * seq
  | For of expr * seq
  | Block of seq
  (* Déclaration de variable locales *)
  | Vars of ident list * typ option * instr list (* ICI *)
  (* Fin d'une fonction *)
  | Return of expr list
  (* Expression utilisée comme instruction *)
  | Expr of expr
```

C'est à dire, `| Vars of ident list * typ option * instr list` au lieu de
`| Vars of ident list * typ option * expr list`. J'ai modifié cela pour me conformer
au sujet.

## Modification de la grammaire pour résoudre les conflits

Dans la grammaire, on a un conflit en ce que j'appellerai le DEFSET (:=) et le SET (=).

Il est dû à ces deux règles :

```
expr: (expr),⁺ = (expr),⁺
expr: (ident),⁺ := (expr),⁺
```

En effet, Menhir étant LL(1), le conflit n'est résolvable qu'à la lecture du 
token DEFSET ou SET, donc potentiellement après plus d'un look-ahead character.

Ma solution a été d'intégrer la vérification des l-values dans la grammaire,
en ajoutant un non-terminal lvalue (cf. `mgoparser.mly`).

Ainsi, les règles pour SET et DEFSET deviennent pareilles avant le token qui 
les différencie, ce qui résout le problème.

Dans le parsing du DEFSET, je rajoute une vérification qui s'assure que toutes 
les l-values fournies ne sont que des identifiants, et les remplace alors par des
expressions «Vars» pour se conformer au type de l'AST.

Je n'ai pas utilisé ma construction «lvalue» dans d'autres règles 
(j'aurais pu pour `simple_instr: e=expr DECR` et `simple_instr: e=expr INCR`), 
car je ne voulais pas trop m'écarter de ce qui était donné par l'énoncé, et aussi
pour éviter la redondance avec le typechecker qui effectue cette vérification de
toute façon (à moins de modifier l'AST)

## Idées d'extensions

### Optimisation des appels récursifs terminaux (TCO)
### Optimisation des appels à des fonctions statiques, à des suites d'appels statiques
### Optimisation des formules logiques
### Optimisation des expressions arithématiques
### Echappement des pointeurs et garbage collector + allocateur
### Détection des fonctions pures et des appels calculables à la compilation (idée de macros)
### Optimisation des registres, des variables utilisées (là on va avoir des algorithmes complexes sur le flot de contrôle)
