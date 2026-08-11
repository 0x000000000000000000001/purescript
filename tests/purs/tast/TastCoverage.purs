module TastCoverage where

import Prelude

-- 1. Data declarations
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- 2. Newtypes
newtype Age = Age Int
derive newtype instance showAge :: Show Age

-- 3. Type classes and instances with superclasses
class Show a <= MyClass a where
  myMethod :: a -> String

instance MyClass Int where
  myMethod i = "Int: " <> show i

instance MyClass Age where
  myMethod (Age a) = "Age: " <> show a

-- 4. Foreign imports
foreign import foreignValue :: forall a. a -> a

-- 5. Records and Updates
type Person = { name :: String, age :: Int }

updatePerson :: Person -> Person
updatePerson p = p { age = p.age + 1 }

-- 6. Do notation with discard (IsSyntheticApp test)
simulateDo :: Effect Unit
simulateDo = do
  log "Hello"
  log "World"
  pure unit

-- Effect mock for the test
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

