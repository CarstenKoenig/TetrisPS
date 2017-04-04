module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (IntervalId, TIMER, setInterval)
import Control.Monad.Except (runExcept)
import Control.Monad.ST (modifySTRef, readSTRef, newSTRef, STRef, ST)
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
import Graphics.Canvas (CANVAS, ScaleTransform, CanvasElement, Context2D, scale, getContext2D, getCanvasElementById, getCanvasHeight, getCanvasWidth, fillRect, setFillStyle)
import Partial.Unsafe (unsafePartial)
import Point (Point, point)
import Tetrominos (Tetromino, rotateTetromino, drawTetromino, tetrominoL)

type GameState s =
  { fallingTetromino :: STRef s FallingTetromino
  }


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


initializeGame :: forall e s. Eff ( st :: ST s | e) (GameState s)                   
initializeGame = do
  tetr <- newSTRef { tetromino: tetrominoL, coord: point 5 0 }
  pure { fallingTetromino: tetr }


drawGame :: forall e s. Context2D -> GameState s -> Eff (canvas :: CANVAS, st :: ST s | e) Unit
drawGame ctx game = do
  setFillStyle "#03101A" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 12.0, h: 20.0 }

  ftetr <- readSTRef game.fallingTetromino
  drawFalling ctx ftetr


drawFalling :: forall e. Context2D -> FallingTetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawFalling ctx ftetr =
    drawTetromino ctx ftetr.coord ftetr.tetromino
  

initializeLoop :: forall e s. Context2D -> GameState s -> Eff (timer :: TIMER, st :: ST s, canvas :: CANVAS | e) IntervalId
initializeLoop ctx game = do
  setInterval 1000 (loop ctx game)


loop :: forall e s. Context2D -> GameState s -> Eff (st :: ST s, canvas :: CANVAS | e) Unit
loop ctx game = do
  modifySTRef game.fallingTetromino drop
  drawGame ctx game


drop :: FallingTetromino -> FallingTetromino
drop fblock = 
  fblock { coord = fblock.coord { y = fblock.coord.y + 1 } }


rotateFalling :: FallingTetromino -> FallingTetromino
rotateFalling ftetr =
  ftetr { tetromino = rotateTetromino ftetr.tetromino } 

moveRight :: FallingTetromino -> FallingTetromino
moveRight fblock = 
  fblock { coord = fblock.coord { x = fblock.coord.x + 1 } }

moveLeft :: FallingTetromino -> FallingTetromino
moveLeft fblock =
  fblock { coord = fblock.coord { x = fblock.coord.x - 1 } }


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
      modifySTRef game.fallingTetromino moveLeft
      drawGame ctx game
    Right "ArrowRight" -> do
      modifySTRef game.fallingTetromino moveRight
      drawGame ctx game
    Right "ArrowDown" -> do
      modifySTRef game.fallingTetromino drop
      drawGame ctx game
    Right "ArrowUp" -> do
      modifySTRef game.fallingTetromino rotateFalling      
      drawGame ctx game
    _ -> 
      pure unit


-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: forall e. Int -> Int -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling w h canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }


main :: forall e s. Eff ( canvas :: CANVAS, st :: ST s, timer :: TIMER, dom :: DOM , console :: CONSOLE| e) Unit
main = void $ unsafePartial do
    game <- initializeGame
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling 12 20 canvas
    scale scaling ctx

    initializeInput ctx game
    loopId <- initializeLoop ctx game
    drawGame ctx game

