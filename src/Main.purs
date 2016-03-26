module Main where

import Prelude (bind, const, ($), (<$>), Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Timer (TIMER)
import DOM (DOM)

import Signal (runSignal, foldp) as S

import Script (Command, Script, fadeOut, clearArt, quote, sprite, text, background)

import Input as Input

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, timer :: TIMER | e) Unit
main = do
  inn <- Input.input
  let game = S.foldp update [] inn
  S.runSignal ((const $ log "hi") <$> game)

script :: Script Command
script = do
  text "Hello,"
  background "bg.png"
  text "This is the first script I'm going to write"
  sprite "protagonist.png"
  quote "Protagonist" "And this is me."
  quote "Protagonist" "But for now, good bye."
  clearArt fadeOut

update :: Array Input.Input -> Array Input.Input -> Array Input.Input
update x _ = x

