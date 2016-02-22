module Script.Print (print) where

import Prelude
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free
import Data.NaturalTransformation

import Script.Internal.Ast


evalPrint :: forall e. NaturalTransformation CommandF (Eff (console :: CONSOLE | e))
evalPrint (Command c k) = const k <$> log (show c)

print :: forall e a. Script a -> Eff (console :: CONSOLE | e) a
print = foldFree evalPrint
