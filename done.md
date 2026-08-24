# Résumé : Introduction de `TypeApp` dans le TAST v2 (CoreFn)

## Le Problème
Auparavant, le compilateur PureScript effaçait systématiquement les informations d'instanciation de type au niveau des appels de fonctions (type erasure).
Lorsqu'une fonction polymorphe (comme `append` ou `show`) était appelée, le backend recevait uniquement l'application du dictionnaire de type classe, sans connaître le type exact qui avait été instancié. Les backends AOT (comme `gopurs`, `purust`, etc.) devaient donc utiliser des types dynamiques (`interface{}` en Go, ou pointeurs virtuels) pour gérer l'exécution, ce qui nuisait aux performances.

## La Solution
Nous avons introduit un nouveau nœud `TypeApp` au niveau des expressions (`Expr`) dans le CoreFn (l'AST final de PureScript).
Ce nœud représente l'application d'un type à une expression (l'équivalent de la syntaxe `@Type` en PureScript).
Désormais, l'information de type instanciée n'est plus effacée. Le backend sait exactement avec quel type une fonction polymorphe a été appelée, **avant** même de recevoir le dictionnaire de type classe.

## Exemple concret

### Code PureScript
```purescript
"Age: " <> show a
-- Correspond à l'appel : append "Age: " (show a)
```
La fonction `append` a le type `forall a. Semigroup a => a -> a -> a`. Ici, elle est instanciée pour le type `String`.

### Avant (TAST v1 / CoreFn standard)
Le backend recevait `append` directement appliqué à son dictionnaire `semigroupString`. L'information `String` était perdue.
```json
{
  "type": "App",
  "abstraction": {
    "type": "Var",
    "value": { "identifier": "append" }
  },
  "argument": {
    "type": "Var",
    "value": { "identifier": "semigroupString" }
  }
}
```

### Après (TAST v2 avec `TypeApp`)
Le compilateur intercale un nœud `TypeApp` explicite. L'expression `append` reçoit d'abord son paramètre de type (`String`), puis, encapsulé dans un `App`, l'argument classique (le dictionnaire).
```json
{
  "type": "App",
  "abstraction": {
    "type": "TypeApp",
    "expression": {
      "type": "Var",
      "value": { "identifier": "append" }
    },
    "typeArgument": {
      "type": "TypeConstructor",
      "name": "String"
    }
  },
  "argument": {
    "type": "Var",
    "value": { "identifier": "semigroupString" }
  }
}
```

## Conséquences pour les backends AOT (gopurs, purust, etc.)
1. **Monomorphisation Native** : Lorsqu'un parseur de backend rencontre un `TypeApp`, il sait exactement quel type est en jeu. Il peut générer un appel à une fonction monomorphisée stricte (ex: `append_String(x, y)`) au lieu de dépendre d'interfaces génériques.
2. **Élimination des Dictionnaires Dynamiques** : Puisque la fonction appelée est statiquement connue (`append_String`), le backend peut choisir d'ignorer complètement l'application du dictionnaire `semigroupString` (le nœud `App` qui suit le `TypeApp`), car le comportement est déjà compilé en dur.
3. **Ordre de résolution** : Dans le cas de fonctions à multiples variables (ex: `forall a b.`), les nœuds `TypeApp` s'emboîtent. L'ordre des `TypeApp` depuis la `Var` originelle correspond exactement à l'ordre de déclaration des variables dans le `forall` de la signature de la fonction. Le premier `TypeApp` instancie `a`, le second `TypeApp` englobant instancie `b`.
