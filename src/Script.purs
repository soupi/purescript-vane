module Script
  (Script
  ,Scene
  ,Command
  ,Placement
  ,Fade
  ,Option
  ,PF
  ,pf
  ,defaultPF
  ,center
  ,left
  ,fadeOut
  ,fadeIn
  ,noFade
  ,choice
  ,wait
  ,text
  ,quote
  ,art
  ,background
  ,sprite
  ,clearArt
  ,playSound
  ,playMusic
  ,pauseMusic
  ,resumeMusic
  ,switchMusic
  )
where

import Prelude
import Data.Maybe
import Control.Monad.Free

-- import Internal.Ast
import Internal.Ast as Ast

-----------
-- Utils --
-----------

type Script a = Ast.Script a
type Scene a = Ast.Scene a
type Command = Ast.Command
type Placement = Ast.Placement
type Fade = Ast.Fade
type Option = Ast.Option

type PF =
  { place :: Placement
  , fade  :: Maybe Fade
  }

pf :: Placement -> Maybe Fade -> PF
pf p f =
  { place: p, fade: f }

defaultPF :: PF
defaultPF =
  { place: Ast.Center, fade: Nothing }

center :: Placement
center = Ast.Center

left :: Placement
left = Ast.LeftSide

right :: Placement
right = Ast.RightSide

fadeOut :: Maybe Fade
fadeOut = Just Ast.FadeOut

fadeIn :: Maybe Fade
fadeIn = Just Ast.FadeIn

noFade :: Maybe Fade
noFade = Nothing

cmd :: Command -> Script Command
cmd x = liftF $ Ast.Command x x

-----------
-- Texts --
-----------

text :: String -> Script Command
text txt =
  cmd (Ast.Text (Ast.Texts txt))

quote :: String -> String -> Script Command
quote name txt =
  cmd (Ast.Text (Ast.Quote name txt))

---------
-- Art --
---------

background :: String -> Script Command
background imgName =
  cmd (Ast.Art (Ast.Display imgName) Ast.Background Nothing)

art :: String -> PF -> Script Command
art imgName pf =
  cmd $ Ast.Art (Ast.Display imgName) pf.place pf.fade

sprite :: String -> Script Command
sprite imgName =
  cmd $ Ast.Art (Ast.Display imgName) center Nothing

clearArt :: Maybe Fade -> Script Command
clearArt fade = cmd (Ast.Art Ast.Remove Ast.All fade)

-----------
-- Music --
-----------

playMusic :: String -> Maybe Fade -> Script Command
playMusic music fade = cmd (Ast.Music (Ast.PlayMusic music) fade)

playSound :: String -> Script Command
playSound music = cmd (Ast.Music (Ast.PlaySound music) noFade)

pauseMusic :: Maybe Fade -> Script Command
pauseMusic fade = cmd (Ast.Music Ast.Pause fade)

resumeMusic :: Maybe Fade -> Script Command
resumeMusic fade = cmd (Ast.Music Ast.Resume fade)

fadeOutMusic :: Script Command
fadeOutMusic = cmd (Ast.Music Ast.Pause fadeOut)

switchMusic :: String -> Script Command
switchMusic music = do
  pauseMusic fadeOut
  playMusic music fadeIn

-----------
-- Other --
-----------

wait :: Script Command
wait = cmd Ast.WaitForKey

choice :: String -> Array Option -> Script Command
choice name opts = cmd (Ast.Choice name opts)
