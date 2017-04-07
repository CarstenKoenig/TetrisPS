module Tetrominos 
    ( Tetromino
    , FallingTetromino
    , rotateTetromino 
    , drawTetromino
    , randomTetromino
    , points
    , drawBlock
    )
where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Data.Int (toNumber)
import Data.Traversable (for_)
import Data.Tuple (snd, fst)
import Graphics.Canvas (setFillStyle, setLineWidth, setStrokeStyle, CANVAS, Context2D, rect, fillPath, strokePath)
import Point (point, Point, Coord(..), rotate90at, toPoint, fromPoint, translate)


type Tetromino =
  { blocks :: Array Coord
  , color :: String
  , center :: Coord
  }


type FallingTetromino = 
  { tetromino :: Tetromino
  , coord :: Point
  }


tetrominoT :: Tetromino
tetrominoT = 
  mkTetromino "#0000FF" (Coord 0.0 0.0) $
  [ Coord (-1.0) 0.0, Coord 0.0 0.0, Coord 1.0 0.0, Coord 0.0 1.0 ]


tetrominoO :: Tetromino
tetrominoO = 
  mkTetromino "#00FF00" (Coord 0.5 0.5) $
  [ Coord 0.0 0.0, Coord 1.0 0.0, Coord 0.0 1.0, Coord 1.0 1.0 ]


tetrominoL :: Tetromino
tetrominoL = 
  mkTetromino "#FF0000" (Coord 0.0 1.0) $
  [ Coord 0.0 0.0, Coord 0.0 1.0, Coord 0.0 2.0, Coord 1.0 2.0 ]


tetrominoJ :: Tetromino
tetrominoJ = 
  mkTetromino "#AF00AF" (Coord 1.0 1.0) $
  [ Coord 1.0 0.0, Coord 1.0 1.0, Coord 1.0 2.0, Coord 0.0 2.0 ]


tetrominoI :: Tetromino
tetrominoI = 
  mkTetromino "#AFAF00" (Coord 0.0 2.0) $
  [ Coord 0.0 0.0, Coord 0.0 1.0, Coord 0.0 2.0, Coord 0.0 3.0 ]


tetrominoS :: Tetromino
tetrominoS = 
  mkTetromino "#00AFAF" (Coord 0.0 0.0) $
  [ Coord (-1.0) 0.0, Coord 0.0 0.0, Coord 0.0 1.0, Coord 1.0 1.0 ]


tetrominoZ :: Tetromino
tetrominoZ = 
  mkTetromino "#F0AFF0" (Coord 0.0 0.0) $
  [ Coord (1.0) 0.0, Coord 0.0 0.0, Coord 0.0 1.0, Coord (-1.0) 1.0 ]


mkTetromino :: String -> Coord -> Array Coord -> Tetromino
mkTetromino col cen bls = { blocks: bls, color: col, center: cen }


points :: Point -> Tetromino -> Array Point
points pt tetr =
    map (toPoint <<< translate (fromPoint pt)) tetr.blocks


rotateTetromino :: Tetromino -> Tetromino
rotateTetromino tetr =
  tetr { blocks = rotate90at tetr.center <$> tetr.blocks  }


randomTetromino :: forall e. Eff (random :: RANDOM | e) FallingTetromino
randomTetromino = do 
    n <- randomInt 1 7
    pure $ selectTetromino n


selectTetromino :: Int -> FallingTetromino
selectTetromino nr = 
    case nr of 
        1 -> { tetromino: tetrominoL, coord: point 5 (-2) }
        2 -> { tetromino: tetrominoT, coord: point 5 (-1) }
        3 -> { tetromino: tetrominoJ, coord: point 5 (-1) }
        4 -> { tetromino: tetrominoI, coord: point 5 (-3) }
        5 -> { tetromino: tetrominoS, coord: point 5 (-1) }
        6 -> { tetromino: tetrominoZ, coord: point 5 (-1) }
        _ -> { tetromino: tetrominoO, coord: point 5 (-1) }




drawTetromino :: forall e. Context2D -> Point -> Tetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawTetromino ctx pt tetr = do
    setStrokeStyle "#000000" ctx
    setLineWidth 0.1 ctx
    for_ (points pt tetr) (drawBlock ctx tetr.color)
    pure unit


drawBlock :: forall e. Context2D -> String -> Point -> Eff ( canvas :: CANVAS | e ) Unit               
drawBlock ctx col pt = do
    setFillStyle col ctx
    strokePath ctx $
    fillPath ctx $ 
    rect ctx
        { x: toNumber (fst pt)
        , y: toNumber (snd pt)
        , w: 1.0
        , h: 1.0
        }
    pure unit

