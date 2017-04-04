module Game 
    ( GameState
    , GameSettings
    , FallingTetromino 
    , initializeGame
    , updateFallingBlock
    , drawGame 
    , drop 
    , rotate 
    , moveRight 
    , moveLeft
    )
where

import Prelude
import Data.Map as Map
import Control.Monad.Eff (Eff)
import Control.Monad.ST (modifySTRef, ST, STRef, readSTRef, newSTRef)
import Data.BooleanAlgebra (class BooleanAlgebra)
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
    GameState
        { fallingTetromino :: STRef s FallingTetromino
        , occupied :: Map Point Color
        , settings :: GameSettings
        }


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


initializeGame :: forall e s. GameSettings -> Eff ( st :: ST s | e) (GameState s)                   
initializeGame sets = do
  tetr <- newSTRef { tetromino: tetrominoL, coord: point 5 (-2) }
  pure $ GameState { fallingTetromino: tetr, settings: sets, occupied: Map.empty }


updateFallingBlock :: forall s e . GameState s -> (FallingTetromino -> FallingTetromino) -> Eff (st :: ST s | e) Unit
updateFallingBlock (GameState gameState) update = do
    modifySTRef gameState.fallingTetromino update'
    pure unit
    where 
        update' ftetr = 
            let ftetr' = update ftetr
            in if isValid gameState.settings ftetr' then ftetr' else ftetr


isValid :: GameSettings -> FallingTetromino -> Boolean
isValid settings ftetr = 
    all (insideBounds settings) (points ftetr.coord ftetr.tetromino)


isOccupied :: forall s. GameState s -> Point -> Boolean
isOccupied (GameState gameState) pt = 
    Map.member pt gameState.occupied

insideBounds :: GameSettings -> Point -> Boolean
insideBounds { rows: r, cols: c } (Tuple x y) = 
    x >= 0 && x < c &&
    y < r    


drawGame :: forall e s. Context2D -> GameState s -> Eff (canvas :: CANVAS, st :: ST s | e) Unit
drawGame ctx (GameState game) = do
  setFillStyle "#03101A" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 12.0, h: 20.0 }

  ftetr <- readSTRef game.fallingTetromino
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

