module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Graphics.Canvas (scale, CanvasElement, ScaleTransform, CANVAS, rect, fillPath, setFillStyle, getContext2D, getCanvasElementById, getCanvasHeight, getCanvasWidth)
import Partial.Unsafe (unsafePartial)

-- Canvas should have Dimension 480x800 so if you scale with 40
-- you get a grid of 12x20

calculateScaling :: forall e. Int -> Int -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling w h canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }


main :: forall e. Eff ( canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling 12 20 canvas
    scale scaling ctx

    setFillStyle "#0000FF" ctx

    fillPath ctx $ rect ctx
      { x: 2.0
      , y: 2.0
      , w: 1.0
      , h: 1.0
      }


    fillPath ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , w: 1.0
      , h: 1.0
      }

    fillPath ctx $ rect ctx
      { x: 11.0
      , y: 0.0
      , w: 1.0
      , h: 1.0
      }
      
