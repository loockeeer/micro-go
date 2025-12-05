# Micro-Go Compiler

Mon compte-rendu : le plus récent en haut, le plus vieux en bas.
Le formattage est un peu étrange car j'ai tout écrit en Markdown en limitant à
80 caractères les lignes, donc la conversion en PDF n'est pas parfaite.

J'ai ajouté quelques fonctionnalités non demandées (elles sont détaillées plus bas):

- Le «Unknown X. Did you mean Y» avec la distance de Levensthein  
- L'utilisation de plus de registres au lieu de la pile
- Une représentation intermédiaire pour faciliter la compilation. Mais il me
    semble que c'était encouragé par le sujet qui suggérait de modifier l'AST
    pour simplifier la compilation et surtout les fonction à plusieurs sorties.
    Mon IR fait essentiellement ceci en ajoutant le support des pointeurs.

## Ajout d'une optimisation des registres utilisés dans le calcul des expressions

Initialement, il est prévu de mettre tous les résultats intermédiaires dans la pile,
mais j'ai ajouté les fonctions `vpush` et `vpop` dans `mips.ml` qui utilisent 
6 registres (`$t1` ... `$t7`) pour remplacer la pile (avant de repartir sur la pile
si il faut plus de place).

## Ajout de fonctions de concaténation de l'assembleur plus pratiques

Durant la phase de compilation, lorsque l'on trouve une chaîne, il faut l'ajouter
au segment de données du programme.
J'ai décidé de «collecter» le segment de données pendant la compilation, ce qui
implique donc de modifier la fonction de concaténation `@@` pour être plus efficace.
J'ai ajouté :
```ocaml
let ($@@) (x:asm * asm) y = C (fst x, y), snd x
let (@@$) x (y: asm * asm) = C (x, fst y), snd y
let ($@@$) (x: asm * asm) (y: asm * asm) = C (fst x, fst y), C (snd x, snd y)
```

Le placement du `$` détermine le côté duquel on s'attend à recevoir un couple
`text * data`. C'est donc assez intuitif, et ça me permet de garder une écriture
concise dans `compile.ml` sans avoir à trop modifier `mips.ml` (j'aurais aussi
pu modifier tout mips.ml pour faire que chaque fonction renvoie un tuple, et
j'aurai ensuite modifié `@@` pour lui faire concaténer les tuples. Mais j'ai
préféré l'autre solution)

## Représentation intermédiaire (conversion pendant le typechecking)

Afin de faciliter la compilation, j'ai ajouté une représentation intermédiaire
dans `ir.ml` :

```ocaml
open Mgoast

type expr =
  | Int of int64
  | Bool of bool
  | String of string
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Var of string
  | DerefShift of expr * int
  | New of int
  | Call of string * expr list
  | Print of expr list
  | Nil
  | Dummy (* variable spéciale *)

and instr =
  | If of expr * seq * seq
  | For of expr * seq
  | Block of seq
  | Inc of expr
  | Dec of expr
  | SetVariable of string * expr
  | SetRefShift of expr * int * expr
  | Return of expr option
  | Expr of expr

and seq = instr list

(* GetReg/SetReg permettent des optimisations mineures *)
type func_def =
  { fname : string
  ; params : string list
  ; locals : string list
  ; body : seq
  }

type program = func_def list
```

### Pointeurs
Elle ajoute le support des pointeurs, pour gérer de façon plus simple le retour
des fonctions à plusieurs sorties, ainsi que pour abstraire le fonctionnement
des structures dans le compilateur (le typechecker change un `Dot(e, i)` en un
`DerefShift(e, shift(i))` où `shift(i)` est connu à la compilation, par exemple).

### Variable globale Dummy
De plus, l'ajout de l'expression `Dummy` (qui s'évalue en fait à label global),
permet d'ignorer efficacement des valeurs lors de l'appel à f.
Ignorer leur calcul n'était pas envisageable car le langage n'est pas purement
fonctionnel, et donc je ne peux pas ignorer une sortie à cause de la possibilité
d'avoir des effets.


### Modification d'un type dans le typechecker pour l'abstraction des structures
Afin de permettre d'abstraire les structures pendant la compilation, j'ai modifié
l'environnement des structures dans le typechecker en ajoutant le décalage (en
nombre d'octets) pour chaque champ :
```ocaml
type senv = (ident * typ * int) list (* l'entier à la fin = le décalage pour
ce champ *)
```

## Type de retour de check_instr et check_seq

Comme on peut déclarer des variables qui vont changer l'environnement,
check_instr et check_seq retournent l'environnement tel que modifié après leur
vérification. Je me sers ensuite de cet environnement pour remplir le champ
« locals ».

## Modification du type tenv

J'ai remarqué que `tenv` ne contenait pas le corps de la fonction à vérifier
dans le typechecker, et j'ai donc rajouté ça simplement.

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

## Ajout d'un terminal «NEW»

Pour gérer correctement l'appel `new(s)`, j'ai ajouté un symbole terminal de 
mot-clé NEW (pour "new"), et une règle dans le parser.
C'est très similaire à ce qui est fait pour le print

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
