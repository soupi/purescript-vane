module Main where

import Prelude (bind, ($), (<$>), Unit)
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
  let game = S.foldp update initState inn
  S.runSignal (view <$> game)

view :: forall e. Input.Input -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
view input = log $ Input.showInput input

script :: Script Command
script = do
  text "Hello,"
  background "bg.png"
  text "This is the first script I'm going to write"
  sprite "protagonist.png"
  quote "Protagonist" "And this is me."
  quote "Protagonist" "But for now, good bye."
  clearArt fadeOut

update :: Input.Input -> Input.Input -> Input.Input
update x _ = x

initState :: Input.Input
initState = Input.initInput
