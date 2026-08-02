module Main where

import Prelude
import Effect (Effect)

foreign import logInt :: Int -> Effect Unit

loopTco :: Int -> Int
loopTco 0 = 0
loopTco n = loopTco (n - 1)

main :: Effect Unit
main = logInt (loopTco 100000)
