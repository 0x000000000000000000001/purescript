module TastCoverage where

import Prelude

-- Data declarations and constructor fields.
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Newtypes and derived instances.
newtype Age = Age Int
derive newtype instance showAge :: Show Age

-- Type classes and instances with superclasses.
class Show a <= MyClass a where
  myMethod :: a -> String

instance MyClass Int where
  myMethod i = "Int: " <> show i

instance MyClass Age where
  myMethod (Age a) = "Age: " <> show a

-- Polymorphic foreign imports.
foreign import foreignValue :: forall a. a -> a

-- Records and updates.
type Person = { name :: String, age :: Int }

updatePerson :: Person -> Person
updatePerson p = p { age = p.age + 1 }

-- Do notation inserts discard applications during desugaring.
simulateDo :: Effect Unit
simulateDo = do
  log "Hello"
  log "World"
  pure unit

-- These effect operations are only compiled, not executed by this fixture.
foreign import data Effect :: Type -> Type
foreign import log :: String -> Effect Unit

foreign import magic :: forall a. a

instance Functor Effect where
  map _ _ = magic

instance Apply Effect where
  apply _ _ = magic

instance Applicative Effect where
  pure _ = magic

instance Bind Effect where
  bind _ _ = magic

instance Monad Effect
