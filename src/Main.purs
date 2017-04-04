module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (IntervalId, TIMER, setInterval)
import Control.Monad.Except (runExcept)
import Control.Monad.ST (ST)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.KeyboardEvent (code, eventToKeyboardEvent)
import DOM.Event.Types (EventType(EventType))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import Data.Either (Either(Right))
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Game (GameState, GameSettings, moveLeft, moveRight, rotate, drawGame, initializeGame, updateMovingBlock, updateFalling)
import Graphics.Canvas (ScaleTransform, CANVAS, CanvasElement, Context2D, getCanvasHeight, getCanvasWidth, scale, getContext2D, getCanvasElementById)
import Partial.Unsafe (unsafePartial)


main :: forall e s. Eff ( canvas :: CANVAS, st :: ST s, timer :: TIMER, dom :: DOM , console :: CONSOLE| e) Unit
main = void $ unsafePartial do
    let settings = { rows: 20, cols: 12}
    game <- initializeGame settings
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling settings canvas
    scale scaling ctx

    initializeInput ctx game
    loopId <- initializeLoop ctx game
    drawGame ctx game



initializeLoop :: forall e s. Context2D -> GameState s -> Eff (timer :: TIMER, st :: ST s, canvas :: CANVAS | e) IntervalId
initializeLoop ctx game = do
  setInterval 1000 (loop ctx game)


loop :: forall e s. Context2D -> GameState s -> Eff (st :: ST s, canvas :: CANVAS | e) Unit
loop ctx game = do
  updateFalling game
  drawGame ctx game


initializeInput :: forall s e. Context2D -> GameState s -> Eff ( dom :: DOM, st :: ST s, canvas :: CANVAS | e) Unit
initializeInput ctx game = do
  w <- window
  addEventListener 
    (EventType "keydown")
    (eventListener $ onKeyPress ctx game)
    false
    (windowToEventTarget w)
  pure unit


onKeyPress :: forall eff s. Context2D -> GameState s -> Event -> Eff ( st :: ST s, canvas :: CANVAS| eff) Unit
onKeyPress ctx game event = do
  case map code (runExcept (eventToKeyboardEvent event)) of
    Right "ArrowLeft" -> do
      updateMovingBlock game moveLeft
      drawGame ctx game
    Right "ArrowRight" -> do
      updateMovingBlock game moveRight
      drawGame ctx game
    Right "ArrowDown" -> do
      updateFalling game
      drawGame ctx game
    Right "ArrowUp" -> do
      updateMovingBlock game rotate
      drawGame ctx game
    _ -> 
      pure unit


-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: forall e. GameSettings -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling { rows: h, cols: w } canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }
