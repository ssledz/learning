module Main where

import Prelude
import Euler (answer)

import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  log ("Answer is: " <> show answer)
