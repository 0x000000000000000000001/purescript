# Intégration de `VisibleTypeApp` (`@Type`) dans l'AST CoreFn

## Contexte
Le compilateur PureScript (fork TAST) a été modifié pour propager les applications de types explicites (Visible Type Applications, ex: `foo @Int`) depuis l'AST source jusqu'à l'AST CoreFn (`tcorefn`), afin de fournir une échappatoire manuelle d'instanciation de type pour les backends AOT (`gopurs`, `purust`).

> **Note importante** : Seules les instanciations *explicites* génèrent un nœud `TypeApp` dans CoreFn. Les instanciations *implicites* (réalisées silencieusement par le TypeChecker) ne génèrent **pas** de `TypeApp` afin d'éviter une explosion de la taille du fichier `corefn.json` (qui causait des OOM sur les parsers JS comme `purs-backend-es`). Pour les types implicites, les backends AOT doivent utiliser la fonction `unify` existante (ex: dans `PureScript.Backend.Optimizer.Substitute`) pour comparer le type générique d'une fonction avec son type instancié (`ann.type` dans TAST v2).

## Modifications apportées

1. **`Language.PureScript.CoreFn.Expr`** : Ajout du constructeur `TypeApp (Ann, Expr a) SourceType` au type `Expr a`.
2. **`Language.PureScript.CoreFn.Desugar`** : Modification de `exprToCoreFn` pour transformer `A.VisibleTypeApp` en `E.TypeApp`.
3. **`Language.PureScript.CoreFn.ToJSON`** : Ajout de la sérialisation JSON avec un champ `typeArgument` pour respecter le `camelCase`.
4. **`purescript-backend-optimizer`** : Ajout du support de `ExprTypeApp` dans le décodage JSON (`Json.purs`), la structure AST (`CoreFn.purs`), et l'inférence (`Monomorphize.purs`, `FreeVars.purs`).

## Exemple de code PureScript

```purescript
module Test where

import Prelude

foo :: forall a. a -> a
foo x = x

bar :: Int -> Int
bar = foo @Int
```

## Résultat JSON (`tcorefn`)

Voici à quoi ressemble le nœud sérialisé pour `foo @Int` :

```json
{
  "type": "TypeApp",
  "annotation": {
    "sourceSpan": {
      "start": [9, 7],
      "name": "/path/to/Test.purs",
      "end": [9, 15]
    },
    "type": {
      "type": "Func",
      "argument": {
        "type": "TypeConstructor",
        "name": "Int"
      },
      "return": {
        "type": "TypeConstructor",
        "name": "Int"
      }
    },
    "meta": null
  },
  "expression": {
    "type": "Var",
    "annotation": {
      "sourceSpan": {
        "start": [9, 7],
        "name": "/path/to/Test.purs",
        "end": [9, 10]
      },
      "type": {
        "type": "ForAll",
        "visibility": "TypeVarInvisible",
        "name": "a",
        "body": {
          "type": "Func",
          "argument": {
            "type": "TypeVar",
            "name": "a"
          },
          "return": {
            "type": "TypeVar",
            "name": "a"
          }
        }
      },
      "meta": null
    },
    "value": "Test.foo"
  },
  "typeArgument": {
    "type": "TypeConstructor",
    "name": "Int"
  }
}
```

## Utilisation côté Backend (Ex: `gopurs`)

Pour exploiter ce nouveau constructeur dans un backend via `purescript-backend-optimizer` :

```purescript
import PureScript.Backend.Optimizer.CoreFn (Expr(..))

-- Dans la passe d'analyse, d'optimisation, ou de génération :
go expr = case expr of
  ExprTypeApp ann expr typeArg ->
    -- 'expr' est l'expression d'origine (ex: le Var "foo")
    -- 'typeArg' est le type explicitement appliqué (ex: Int)
    -- On délègue ou on utilise l'information pour forcer la monomorphisation manuelle.
    go expr
```
