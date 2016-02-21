module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Script

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello vane!"


script :: Script Command
script = do
  text "Hello,"
  background "bg.png"
  text "This is the first script I'm going to write"
  sprite "protagonist.png"
  quote "Protagonist" "And this is me."
  quote "Protagonist" "But for now, good bye."
  clearArt fadeOut
