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
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(Just))
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, ScaleTransform, fillPath, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, rect, scale, setFillStyle, setLineWidth, setStrokeStyle, strokePath)
import Partial.Unsafe (unsafePartial)


type GameState s =
  { fallingTetromino :: STRef s FallingTetromino
  }


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Coord
  }


type Tetromino =
  { blocks :: Array Coord
  , color :: String
  , center :: CoordN
  }


tetromino :: String -> CoordN -> Array Coord -> Tetromino
tetromino col cen bls = { blocks: bls, color: col, center: cen }


tetrominoT :: Tetromino
tetrominoT = 
  tetromino "#0000FF" (coordN 0.0 0.0) $
  [ coord (-1) 0, coord 0 0, coord 1 0, coord 0 1 ]


tetrominoO :: Tetromino
tetrominoO = 
  tetromino "#00FF00" (coordN 0.5 0.5) $
  [ coord 0 0, coord 1 0, coord 0 1, coord 1 1 ]


tetrominoL :: Tetromino
tetrominoL = 
  tetromino "#FF0000" (coordN 0.0 1.0) $
  [ coord 0 0, coord 0 1, coord 0 2, coord 1 2 ]


type Coord = 
  { x :: Int
  , y :: Int
  }


type CoordN = 
  { x :: Number
  , y :: Number
  }    


coord :: Int -> Int -> Coord
coord x y = { x: x, y: y }


coordN :: Number -> Number -> CoordN
coordN x y = { x: x, y: y }


toCoord :: CoordN -> Coord
toCoord c = coord (floor c.x) (floor c.y)


toCoordN :: Coord -> CoordN
toCoordN c = coordN (toNumber c.x) (toNumber c.y)


rotate90at :: CoordN -> Coord -> Coord
rotate90at { x:cx, y:cy } = 
  toCoord <<< 
  translate (coordN cx cy) <<< rotate90 <<< translate (coordN (-cx) (-cy)) <<< 
  toCoordN


translate :: CoordN -> CoordN -> CoordN 
translate { x: tx, y: ty } { x: x, y: y } = 
  coordN (x+tx) (y+ty)


rotate90 :: CoordN -> CoordN
rotate90 { x:x, y:y } = 
  coordN (-y) x


initializeGame :: forall e s. Eff ( st :: ST s | e) (GameState s)                   
initializeGame = do
  tetr <- newSTRef { tetromino: tetrominoL, coord: coord 5 0 }
  pure { fallingTetromino: tetr }


drawGame :: forall e s. Context2D -> GameState s -> Eff (canvas :: CANVAS, st :: ST s | e) Unit
drawGame ctx game = do
  setFillStyle "#03101A" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 12.0, h: 20.0 }

  block <- readSTRef game.fallingTetromino
  drawFalling ctx block


drawFalling :: forall e. Context2D -> FallingTetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawFalling ctx tetr = do
    let draw {x: x, y: y} = 
          strokePath ctx $
          fillPath ctx $ rect ctx
            { x: toNumber (x + tetr.coord.x)
            , y: toNumber (y + tetr.coord.y)
            , w: 1.0
            , h: 1.0
            }
    setStrokeStyle "#000000" ctx
    setLineWidth 0.1 ctx
    setFillStyle tetr.tetromino.color ctx
    for_ tetr.tetromino.blocks draw
    pure unit
  

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


rotateTetromino :: FallingTetromino -> FallingTetromino
rotateTetromino ftetr = 
  ftetr { tetromino { blocks = rotate90at ftetr.tetromino.center <$> ftetr.tetromino.blocks  } } 

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
      modifySTRef game.fallingTetromino rotateTetromino      
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

