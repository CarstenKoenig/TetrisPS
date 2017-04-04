module Game 
    ( GameState
    , GameSettings
    , FallingTetromino 
    , Color
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
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Map (Map)
import Data.Traversable (all)
import Data.Tuple (snd, Tuple(Tuple), fst)
import Graphics.Canvas (CANVAS, Context2D, fillRect, setFillStyle)
import Point (Point, point)
import Tetrominos (Tetromino, drawTetromino, points, rotateTetromino, tetrominoL, tetrominoO, tetrominoT)


type Color = String


type GameSettings = 
    { rows :: Int
    , cols :: Int
    }

type GameState =
    { fallingTetromino :: FallingTetromino
    , occupied :: Map Point Color
    , settings :: GameSettings
    }


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


initializeGame :: GameSettings -> GameState
initializeGame sets = do
    let ftetr = { tetromino: tetrominoL, coord: point 5 (-2) }
    { fallingTetromino: ftetr, occupied: (Map.empty :: Map Point Color), settings: sets }


updateMovingBlock :: (FallingTetromino -> FallingTetromino) -> GameState -> GameState
updateMovingBlock update gamestate =
    let ftetr = update' gamestate.fallingTetromino
    in gamestate { fallingTetromino = ftetr }
    where 
        update' ftetr =
            let ftetr' = update ftetr
                valid = isValid gamestate ftetr'
            in if valid then ftetr' else ftetr


updateFalling :: forall e. GameState -> Eff (random :: RANDOM | e) GameState
updateFalling gamestate = do
    let falling = gamestate.fallingTetromino
        dropped = drop falling
        valid = isValid gamestate dropped
    if valid
        then
            pure $ gamestate { fallingTetromino = dropped }
        else do 
            gamestate' <- initNewFalling gamestate 
            pure $ stopFalling gamestate'
            

stopFalling :: GameState -> GameState
stopFalling gs = gs


initNewFalling :: forall e. GameState -> Eff (random :: RANDOM | e) GameState
initNewFalling gamestate = do
    ftetr <- randomTetromino
    pure $ gamestate { fallingTetromino = ftetr }


randomTetromino :: forall e. Eff (random :: RANDOM | e) FallingTetromino
randomTetromino = do 
    n <- randomInt 1 3
    pure $ selectTetromino n


selectTetromino :: Int -> FallingTetromino
selectTetromino nr = 
    case nr of 
        1 -> { tetromino: tetrominoL, coord: point 5 (-2) }
        2 -> { tetromino: tetrominoT, coord: point 5 (-1) }
        _ -> { tetromino: tetrominoO, coord: point 5 (-1) }


isValid :: GameState -> FallingTetromino ->  Boolean
isValid gamestate ftetr =
    all (valid gamestate.settings gamestate.occupied) (points ftetr.coord ftetr.tetromino)
    where 
        valid setts map pt =
            insideBounds setts pt && not (isOccupied map pt)


isOccupied :: Map Point Color -> Point -> Boolean
isOccupied map pt =
    Map.member pt map


insideBounds :: GameSettings -> Point -> Boolean
insideBounds { rows: r, cols: c } (Tuple x y) = 
    x >= 0 && x < c &&
    y < r    


drawGame :: forall e. Context2D -> GameState -> Eff (canvas :: CANVAS | e) Unit
drawGame ctx gamestate = do
  setFillStyle "#03101A" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 12.0, h: 20.0 }
  let ftetr = gamestate.fallingTetromino
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

