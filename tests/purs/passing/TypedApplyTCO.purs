module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)

-- Type applications on ($) and (#) must not hide these tail calls.
countApply :: Int -> Int -> Int
countApply remaining count
  | remaining <= 0 = count
  | otherwise = countApply (remaining - 1) $ count + 1

countFlipped :: Int -> Int -> Int
countFlipped remaining count
  | remaining <= 0 = count
  | otherwise = (count + 1) # countFlipped (remaining - 1)

main :: Effect Unit
main = do
  assertEqual { expected: 100000, actual: countApply 100000 0 }
  assertEqual { expected: 100000, actual: countFlipped 100000 0 }
  log "Done"
