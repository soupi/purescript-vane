module Script.Internal.Ast where

import Prelude
import Data.Maybe
import Control.Monad.Free
import Data.Generic


type Scene a
  = Script a

type Script a = Free CommandF a

data CommandF r
  = Command Command r

instance functorCommandF :: Functor CommandF where
  map f (Command c r) = Command c (f r)

data Command
  = Text  TextCommand
  | Music MusicCommand (Maybe Fade)
  | Art   ArtCommand Placement (Maybe Fade)
  | Choice String (Array Option)
  | WaitForKey

data TextCommand
  = Texts String
  | Quote String String

data MusicCommand
  = PlayMusic String
  | PlaySound String
  | Pause
  | Resume

data Placement
  = Background
  | LeftSide
  | RightSide
  | Center
  | Top
  | Bottom
  | All

data ArtCommand
  = Display String
  | Remove

data Fade
  = FadeIn
  | FadeOut

type Option = String

derive instance genericFade :: Generic Fade
derive instance genericPlacement :: Generic Placement
derive instance genericTextCommand :: Generic TextCommand
derive instance genericMusicCommand :: Generic MusicCommand
derive instance genericArtCommand :: Generic ArtCommand
derive instance genericCommand :: Generic Command
derive instance genericCommandF :: Generic r => Generic (CommandF r)

instance showFade :: Show Fade where
  show = gShow
instance showPlacement :: Show Placement where
  show = gShow
instance showTextCommand :: Show TextCommand where
  show = gShow
instance showMusicCommand :: Show MusicCommand where
  show = gShow
instance showArtCommand :: Show ArtCommand where
  show = gShow
instance showCommand :: Show Command where
  show = gShow
instance showCommandF :: (Generic r, Show r) => Show (CommandF r) where
  show = gShow
