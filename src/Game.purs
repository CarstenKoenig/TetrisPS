module Game 
    ( GameState
    , GameSettings
    , FallingTetromino 
    , initializeGame
    , updateMovingBlock
    , updateFalling
    , drawGame 
    , rotate 
    , moveRight 
    , moveLeft
    )
where

import Prelude
import Data.Map as Map
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import Data.Map (Map)
import Data.Traversable (all)
import Data.Tuple (snd, Tuple(Tuple), fst)
import Graphics.Canvas (CANVAS, Context2D, fillRect, setFillStyle)
import Point (Point, point)
import Tetrominos (Tetromino, rotateTetromino, drawTetromino, tetrominoL, points)


type Color = String


type GameSettings = 
    { rows :: Int
    , cols :: Int
    }

data GameState s =
    GameState (STRef s 
        { fallingTetromino :: FallingTetromino
        , occupied :: Map Point Color
        , settings :: GameSettings
        })


fallingTetromino :: forall s e. GameState s -> Eff ( st :: ST s | e) FallingTetromino
fallingTetromino (GameState gs) = do
    gamestate <- readSTRef gs
    pure $ gamestate.fallingTetromino


setFallingTetromino :: forall s e. GameState s -> FallingTetromino -> Eff ( st :: ST s | e) Unit
setFallingTetromino (GameState gs) ftetr = do
    gamestate <- readSTRef gs
    let gamestate' = gamestate { fallingTetromino = ftetr }
    writeSTRef gs gamestate'
    pure unit


occupied :: forall s e. GameState s -> Eff ( st :: ST s | e) (Map Point Color)
occupied (GameState gs) = do
    gamestate <- readSTRef gs
    pure $ gamestate.occupied


settings :: forall s e. GameState s -> Eff ( st :: ST s | e) GameSettings
settings (GameState gs) = do
    gamestate <- readSTRef gs
    pure $ gamestate.settings


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


initializeGame :: forall e s.                                                   
  GameSettings -> Eff ( st :: ST s | e) (GameState s)                                                            
initializeGame sets = do
    let ftetr = { tetromino: tetrominoL, coord: point 5 (-2) }
    state <- newSTRef { fallingTetromino: ftetr, occupied: (Map.empty :: Map Point Color), settings: sets }
    pure $ GameState state


updateMovingBlock :: forall s e . GameState s -> (FallingTetromino -> FallingTetromino) -> Eff (st :: ST s | e) Unit
updateMovingBlock gs update = do
    gamestate <- gameState gs
    ftetr <- update' gamestate.fallingTetromino
    setFallingTetromino gs ftetr  
    where 
        gameState (GameState gs) = readSTRef gs
        update' ftetr = do
            let ftetr' = update ftetr
            valid <- isValid gs ftetr'
            pure $ if valid then ftetr' else ftetr


updateFalling :: forall s e . GameState s -> Eff (st :: ST s | e) Unit
updateFalling gs = do
    gamestate <- gameState gs
    let falling = gamestate.fallingTetromino
    let dropped = drop falling
    valid <- isValid gs dropped
    if valid
        then
            setFallingTetromino gs dropped
        else do
            stopFalling gs
            initNewFalling gs
    where
        gameState (GameState gs) = readSTRef gs
                
            

stopFalling :: forall s e . GameState s -> Eff (st :: ST s | e) Unit
stopFalling gs = pure unit


initNewFalling :: forall s e . GameState s -> Eff (st :: ST s | e) Unit
initNewFalling gs = pure unit


isValid :: forall e s. GameState s -> FallingTetromino ->  Eff (st :: ST s | e) Boolean
isValid gs ftetr = do
    gamestate <- gameState gs
    pure $ all (valid gamestate.settings gamestate.occupied) (points ftetr.coord ftetr.tetromino)
    where 
        gameState (GameState gs) = readSTRef gs
        valid setts map pt =
            insideBounds setts pt && not (isOccupied map pt)


isOccupied :: Map Point Color -> Point -> Boolean
isOccupied map pt =
    Map.member pt map


insideBounds :: GameSettings -> Point -> Boolean
insideBounds { rows: r, cols: c } (Tuple x y) = 
    x >= 0 && x < c &&
    y < r    


drawGame :: forall e s. Context2D -> GameState s -> Eff (canvas :: CANVAS, st :: ST s | e) Unit
drawGame ctx gs = do
  setFillStyle "#03101A" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 12.0, h: 20.0 }
  ftetr <- fallingTetromino gs
  drawFalling ctx ftetr


drop :: FallingTetromino -> FallingTetromino
drop fblock = 
  fblock { coord = Tuple (fst fblock.coord) (snd fblock.coord + 1) }


rotate :: FallingTetromino -> FallingTetromino
rotate ftetr =
  ftetr { tetromino = rotateTetromino ftetr.tetromino } 


moveRight :: FallingTetromino -> FallingTetromino
moveRight fblock = 
  fblock { coord = Tuple (fst fblock.coord + 1) (snd fblock.coord) }


moveLeft :: FallingTetromino -> FallingTetromino
moveLeft fblock =
  fblock { coord = Tuple (fst fblock.coord - 1) (snd fblock.coord) }


drawFalling :: forall e. Context2D -> FallingTetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawFalling ctx ftetr =
    drawTetromino ctx ftetr.coord ftetr.tetromino

