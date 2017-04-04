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
import Control.Monad.Eff (Eff)
import Control.Monad.ST (modifySTRef, ST, STRef, readSTRef, newSTRef)
import Data.Traversable (all)
import Graphics.Canvas (CANVAS, Context2D, fillRect, setFillStyle)
import Point (Point, point)
import Tetrominos (Tetromino, rotateTetromino, drawTetromino, tetrominoL, points)


type GameSettings = 
    { rows :: Int
    , cols :: Int
    }

data GameState s =
    GameState
        { fallingTetromino :: STRef s FallingTetromino
        , settings :: GameSettings
        }


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


initializeGame :: forall e s. GameSettings -> Eff ( st :: ST s | e) (GameState s)                   
initializeGame sets = do
  tetr <- newSTRef { tetromino: tetrominoL, coord: point 5 (-2) }
  pure $ GameState { fallingTetromino: tetr, settings: sets }


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


insideBounds :: GameSettings -> Point -> Boolean
insideBounds { rows: r, cols: c } pt = 
    pt.x >= 0 && pt.x < c &&
    pt.y < r    


drawGame :: forall e s. Context2D -> GameState s -> Eff (canvas :: CANVAS, st :: ST s | e) Unit
drawGame ctx (GameState game) = do
  setFillStyle "#03101A" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 12.0, h: 20.0 }

  ftetr <- readSTRef game.fallingTetromino
  drawFalling ctx ftetr


drop :: FallingTetromino -> FallingTetromino
drop fblock = 
  fblock { coord = fblock.coord { y = fblock.coord.y + 1 } }


rotate :: FallingTetromino -> FallingTetromino
rotate ftetr =
  ftetr { tetromino = rotateTetromino ftetr.tetromino } 


moveRight :: FallingTetromino -> FallingTetromino
moveRight fblock = 
  fblock { coord = fblock.coord { x = fblock.coord.x + 1 } }


moveLeft :: FallingTetromino -> FallingTetromino
moveLeft fblock =
  fblock { coord = fblock.coord { x = fblock.coord.x - 1 } }


drawFalling :: forall e. Context2D -> FallingTetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawFalling ctx ftetr =
    drawTetromino ctx ftetr.coord ftetr.tetromino

