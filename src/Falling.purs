module Falling 
    ( FallingTetromino
    , random
    , drop
    , rotate
    , moveRight
    , moveLeft
    , draw
    )
where

import Tetrominos

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Data.Tuple (Tuple(..), fst, snd)
import Graphics.Canvas (CANVAS, Context2D)
import Point (Point, point)
import Prelude (Unit, bind, negate, pure, ($), (+), (-))

type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


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


draw :: forall e. Context2D -> FallingTetromino -> Eff ( canvas :: CANVAS | e ) Unit               
draw ctx ftetr =
    drawTetromino ctx ftetr.coord ftetr.tetromino


random :: forall e. Eff (random :: RANDOM | e) FallingTetromino
random = do 
    n <- randomInt 1 7
    pure $ select n


select :: Int -> FallingTetromino
select nr = 
    case nr of 
        1 -> { tetromino: tetrominoL, coord: point 5 (-2) }
        2 -> { tetromino: tetrominoT, coord: point 5 (-1) }
        3 -> { tetromino: tetrominoJ, coord: point 5 (-1) }
        4 -> { tetromino: tetrominoI, coord: point 5 (-3) }
        5 -> { tetromino: tetrominoS, coord: point 5 (-1) }
        6 -> { tetromino: tetrominoZ, coord: point 5 (-1) }
        _ -> { tetromino: tetrominoO, coord: point 5 (-1) }
