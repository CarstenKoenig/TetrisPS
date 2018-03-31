module JQuery where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

-- | updates the current score in the DOM
foreign import setScore :: ∀ eff . String -> Eff ( dom :: DOM | eff ) Unit

-- | displays/hides the GameOver overlay
foreign import showGameOver :: ∀ eff . Boolean -> Eff ( dom :: DOM | eff ) Unit

