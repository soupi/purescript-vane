module Main where

import Prelude (pure, bind, ($), (<$>), Unit, unit)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C
import Signal (runSignal, foldp) as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Timer (TIMER)
import DOM (DOM)

import Vane.Script (Command, Script, fadeOut, clearArt, quote, sprite, text, background)
import Vane.Input as Input
import Vane.Utils
import Vane.Graphics.CanvasUtils


----------
-- Glue
----------

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, timer :: TIMER, canvas :: C.Canvas | e) Unit
main = do
  Just canvas <- C.getCanvasElementById "canvas"
  context <- C.getContext2D canvas
  inn <- Input.input
  let game = S.foldp update initState inn
  S.runSignal (render context <$> game)


script :: Script Command
script = do
  text "Hello,"
  background "bg.png"
  text "This is the first script I'm going to write"
  sprite "protagonist.png"
  quote "Protagonist" "And this is me."
  quote "Protagonist" "But for now, good bye."
  clearArt fadeOut


-----------
-- Model
-----------

type State = Input.Input

initState :: State
initState = Input.initInput

------------
-- Update
------------

update :: Input.Input -> Input.Input -> Input.Input
update x _ = x

------------
-- Render
------------

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

