module JQuery where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

foreign import setScore :: forall eff . String -> Eff ( dom :: DOM | eff ) Unit

