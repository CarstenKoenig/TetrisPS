module Tetrominos 
    ( Tetromino
    , rotateTetromino 
    , drawTetromino
    , tetrominoL 
    , tetrominoT
    , tetrominoO
    , points
    )
where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import Data.Traversable (for_)
import Graphics.Canvas (setFillStyle, setLineWidth, setStrokeStyle, CANVAS, Context2D, rect, fillPath, strokePath)
import Point (Point, Coord(..), rotate90at, toPoint, fromPoint, translate)


type Tetromino =
  { blocks :: Array Coord
  , color :: String
  , center :: Coord
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


mkTetromino :: String -> Coord -> Array Coord -> Tetromino
mkTetromino col cen bls = { blocks: bls, color: col, center: cen }


points :: Point -> Tetromino -> Array Point
points pt tetr =
    map (toPoint <<< translate (fromPoint pt)) tetr.blocks

rotateTetromino :: Tetromino -> Tetromino
rotateTetromino tetr =
  tetr { blocks = rotate90at tetr.center <$> tetr.blocks  }

drawTetromino :: forall e. Context2D -> Point -> Tetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawTetromino ctx pt tetr = do
    setStrokeStyle "#000000" ctx
    setLineWidth 0.1 ctx
    setFillStyle tetr.color ctx
    for_ tetr.blocks (drawBlock ctx pt)
    pure unit

drawBlock :: forall e. Context2D -> Point -> Coord -> Eff ( canvas :: CANVAS | e ) Unit               
drawBlock ctx pt (Coord bx by) = do
    strokePath ctx $
    fillPath ctx $ 
    rect ctx
        { x: toNumber pt.x + bx
        , y: toNumber pt.y + by
        , w: 1.0
        , h: 1.0
        }
    pure unit

