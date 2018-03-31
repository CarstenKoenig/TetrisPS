module SignalExt where
  

import Control.Monad.Eff (Eff)
import Signal (Signal)
  

foreign import foldM :: forall a b e. (a -> b -> (Eff e b)) -> b -> (Signal a) -> Eff e (Signal b)