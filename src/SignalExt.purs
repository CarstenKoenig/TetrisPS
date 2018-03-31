module SignalExt where
  

import Control.Monad.Eff (Eff)
import Signal (Signal)
  
-- |effectful foldp
foreign import foldEff :: forall a b e. (a -> b -> (Eff e b)) -> b -> (Signal a) -> Eff e (Signal b)