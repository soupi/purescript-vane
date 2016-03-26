module Input where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Control.Timer (TIMER)
import Data.Int (toNumber)
import Data.Traversable (sequence)

import Data.Array (catMaybes)
import Data.Maybe (Maybe (Just, Nothing), isNothing)

import Signal as S
import Signal.DOM (keyPressed, mouseButton, mousePos, animationFrame) as S

import CanvasUtils (Point)

data Input
  = Arrows (Array Arrow)
  | MouseClick Point
  | Tick

data Arrow
  = LeftArrow
  | RightArrow
  | UpArrow
  | DownArrow

input :: forall e. Eff (dom :: DOM, timer :: TIMER | e) (S.Signal (Array Input))
input = do
  frames <- S.animationFrame
  arrowsInputs <- arrows
  mouse <- mouseClick
  pure (sequence [arrowsInputs, mouse, map (const Tick) frames])

mouseClick :: forall e. Eff (dom :: DOM | e) (S.Signal Input)
mouseClick = do
  pos  <- S.mousePos
  down <- S.filter id false <<< S.dropRepeats <$> S.mouseButton 0
  let sig = S.sampleOn down ((\p -> { x: toNumber p.x, y: toNumber p.y }) <$> pos)
  pure $ map MouseClick sig

arrows :: forall e. Eff (dom :: DOM | e) (S.Signal Input)
arrows = do
  leftInput  <- S.filter isNothing Nothing <<< map (\x -> if x then Just LeftArrow else Nothing)  <<< S.dropRepeats <$> S.keyPressed leftKeyCode
  rightInput <- S.filter isNothing Nothing <<< map (\x -> if x then Just RightArrow else Nothing) <<< S.dropRepeats <$> S.keyPressed rightKeyCode
  upInput    <- S.filter isNothing Nothing <<< map (\x -> if x then Just UpArrow else Nothing)    <<< S.dropRepeats <$> S.keyPressed upKeyCode
  downInput  <- S.filter isNothing Nothing <<< map (\x -> if x then Just DownArrow else Nothing)  <<< S.dropRepeats <$> S.keyPressed downKeyCode
  let arrs = sequence [leftInput, rightInput, downInput, upInput]
  pure $ map (Arrows <<< catMaybes) arrs

leftKeyCode :: Int
leftKeyCode = 37

rightKeyCode :: Int
rightKeyCode = 39

upKeyCode :: Int
upKeyCode = 38

downKeyCode :: Int
downKeyCode = 40

asNum :: Boolean -> Number
asNum b = if b then 1.0 else 0.0

