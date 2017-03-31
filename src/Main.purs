module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.ST (readSTRef, newSTRef, STRef, ST)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.Unit (Unit)
import Graphics.Canvas (Context2D, CanvasElement, ScaleTransform, CANVAS, scale, rect, fillPath, setFillStyle, getContext2D, getCanvasElementById, getCanvasHeight, getCanvasWidth)
import Partial.Unsafe (unsafePartial)


type FallingBlock = 
  { posX :: Int
  , posY :: Int
  }


type GameState s =
  { fallingBlock :: STRef s FallingBlock
  }


initializeGame :: forall e s. Eff ( st :: ST s | e) (GameState s)                   
initializeGame = do
  block <- newSTRef { posX: 5, posY: 0 }
  pure { fallingBlock: block }


drawGame :: forall e s. Context2D -> GameState s -> Eff (canvas :: CANVAS, st :: ST s | e) Unit
drawGame ctx game = do
  block <- readSTRef game.fallingBlock
  drawFalling ctx block


drawFalling :: forall e. Context2D -> FallingBlock -> Eff ( canvas :: CANVAS | e ) Unit               
drawFalling ctx block = do
    setFillStyle "#0000FF" ctx
    fillPath ctx $ rect ctx
      { x: toNumber block.posX
      , y: toNumber block.posY
      , w: 1.0
      , h: 1.0
      }
    pure unit
  

-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: forall e. Int -> Int -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling w h canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }


main :: forall e s. Eff ( canvas :: CANVAS, st :: ST s | e) Unit
main = void $ unsafePartial do
    game <- initializeGame

    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling 12 20 canvas
    scale scaling ctx

    drawGame ctx game