module Script where

import Prelude
import Data.Maybe
import Control.Monad.Free

import Ast

-----------
-- Utils --
-----------

type PF =
  { place :: Placement
  , fade  :: Maybe Fade
  }

pf :: Placement -> Maybe Fade -> PF
pf p f =
  { place: p, fade: f }

defaultPF :: PF
defaultPF =
  { place: Center, fade: Nothing }

center :: Placement
center = Center

left :: Placement
left = LeftSide

right :: Placement
right = RightSide

fadeOut :: Maybe Fade
fadeOut = Just FadeOut

fadeIn :: Maybe Fade
fadeIn = Just FadeIn

noFade :: Maybe Fade
noFade = Nothing

cmd :: Command -> Script Command
cmd x = liftF $ Command x x

-----------
-- Texts --
-----------

text :: String -> Script Command
text txt =
  cmd (Text (Texts txt))

quote :: String -> String -> Script Command
quote name txt =
  cmd (Text (Quote name txt))

---------
-- Art --
---------

bg :: String -> Script Command
bg imgName =
  cmd (Art (Display imgName) Background Nothing)

art :: String -> PF -> Script Command
art imgName pf =
  cmd $ Art (Display imgName) pf.place pf.fade

sprite :: String -> Script Command
sprite imgName =
  cmd $ Art (Display imgName) center Nothing

clearArt :: Maybe Fade -> Script Command
clearArt fade = cmd (Art Remove All fade)

-----------
-- Music --
-----------

playMusic :: String -> Maybe Fade -> Script Command
playMusic music fade = cmd (Music (PlayMusic music) fade)

playSound :: String -> Script Command
playSound music = cmd (Music (PlaySound music) noFade)

pauseMusic :: Maybe Fade -> Script Command
pauseMusic fade = cmd (Music Pause fade)

resumeMusic :: Maybe Fade -> Script Command
resumeMusic fade = cmd (Music Resume fade)

fadeOutMusic :: Script Command
fadeOutMusic = cmd (Music Pause fadeOut)

switchMusic :: String -> Script Command
switchMusic music = do
  pauseMusic fadeOut
  playMusic music fadeIn

-----------
-- Other --
-----------

wait :: Script Command
wait = cmd WaitForKey

choice :: String -> Array Option -> Script Command
choice name opts = cmd (Choice name opts)
