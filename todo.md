# Roadmap: Émission des arguments génériques (TypeApp) dans le TAST

## 1. Le Problème Actuel

Dans le compilateur PureScript (ainsi que dans le fork TAST `tcorefn` actuel), l'application des types est **implicite**. 
Lorsqu'une fonction polymorphe est appelée avec un type concret (ex: `identity 42`), l'arbre syntaxique abstrait (AST) génère un simple nœud d'application (`App`) liant la variable `identity` à l'argument `42`.

Bien que ce fork enrichisse intelligemment chaque nœud avec son type résultant final (via la propriété `ann.type`), **le TAST ne fournit nulle part la liste explicite des arguments génériques (les types concrets) qui ont été substitués aux variables de type au point d'appel**. 
Il manque un nœud de type `TypeApp` (concept issu de System F) qui dirait explicitement au backend : *"Ici, on appelle `identity` en unifiant la variable générique `a` avec `Int`"*.

## 2. Conséquences sur les Backends AOT actuels

Cette absence de "Type Applications" explicites bloque le plein potentiel des compilateurs AOT (Ahead-of-Time) :

- **Rust (`purust`)** : L'impossibilité de fournir les paramètres génériques exacts via la syntaxe turbofish (`identity::<i32>(42)`) provoque des erreurs d'inférence sévères (`E0282: type annotations needed`). Pour éviter que le compilateur Rust ne panique, le backend est contraint de masquer les types derrière un type dynamique unifié (`Value` / `Box<dyn Any>`), ce qui tue les performances (allocations sur la heap, indirections, boxing systématique).
- **Go (`gopurs`)** : Impossible d'utiliser les vrais génériques natifs de Go 1.18 (`func Identity[T any]`). Le code généré s'appuie massivement sur `interface{}` (`any`). Conséquence : dès qu'une primitive (`int`, `bool`...) est passée à une fonction polymorphe, le runtime Go doit l'allouer sur la heap (*boxing*), entraînant un surcoût colossal pour le Garbage Collector.
- **L'Optimiseur (`purescript-backend-optimizer`)** : Sans connaître les arguments de type exacts appliqués aux fonctions, la passe de monomorphisation doit recourir à des heuristiques fragiles pour "deviner" l'instanciation depuis le type global de l'expression. Une application de type explicite rendrait la monomorphisation robuste, systématique et triviale à implémenter.

## 3. L'Objectif : Le "Saint Graal" de la compilation AOT

Émettre les `TypeApp` explicitement dans le TAST transformerait fondamentalement l'écosystème : on passerait d'un *"langage dynamique compilé astucieusement vers un langage typé"* à un **"langage strictement typé et monomorphisé de bout en bout"**.

**Bénéfices attendus :**
- **Rust** : Éradication totale du type dynamique `Value`. Le code généré utiliserait les génériques natifs de Rust (ou serait 100% monomorphisé), permettant le stockage intégral sur la pile (stack) avec 0 overhead d'exécution.
- **Go** : Utilisation des génériques Go, fin du boxing des primitives (zéro allocation GC pour le passage de valeurs simples aux fonctions polymorphes).
- **Performances** : Inlining ultra-agressif par LLVM / le backend cible, avec des performances rivalisant avec du code C/Rust écrit à la main.

## 4. Pistes d'Implémentation (dans `htdocs/purescript`)

Le défi est colossal, car il nécessite de modifier le cœur du TypeChecker Haskell de PureScript.

1. **Intercepter l'Instanciation / Subsomption** :
   Dans le `TypeChecker` de PureScript (notamment lors du traitement de la subsomption et de la "skolemisation"), l'algorithme unifie des inconnues (`?0`) avec des types concrets (`Int`). Actuellement, l'algorithme effectue cette vérification puis "oublie" l'information d'instanciation au niveau de l'AST élaboré. Il faut capturer ces unifications.
2. **Modifier l'AST Élaboré (`Language.PureScript.AST`)** :
   Introduire une nouvelle construction dans l'AST pour stocker ces arguments de type résolus (ex: enrichir l'expression `Var` ou introduire un nœud d'expression `TypeApp`), afin que l'information survive au TypeChecker.
3. **Mise à jour du Desugarer (`Language.PureScript.CoreFn.Desugar`)** :
   Propager ces nouveaux `TypeApp` depuis l'AST élaboré vers le TAST JSON final (`tcorefn`), pour que `purescript-backend-optimizer` puisse enfin les consommer.

> **Difficulté estimée : Extrême.** Cette roadmap requiert une compréhension chirurgicale de l'algorithme d'unification de PureScript et une modification profonde de la façon dont l'AST est annoté et renvoyé post-TypeChecking.
