module Tetrominos 
    ( Tetromino
    , rotateTetromino 
    , drawTetromino
    , points
    , drawBlock
    , tetrominoI
    , tetrominoJ
    , tetrominoL
    , tetrominoO
    , tetrominoS
    , tetrominoT
    , tetrominoZ
    )
where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import Data.Traversable (for_)
import Data.Tuple (snd, fst)
import Graphics.Canvas (setFillStyle, setLineWidth, setStrokeStyle, CANVAS, Context2D, rect, fillPath, strokePath)
import Point (Coord(Coord), Point, fromPoint, rotate90at, toPoint, translate)


-- | represents a (usually falling) Tetris-piece
-- | the inlcudes `Coords` are not representing screen-coordinates
-- | but relative positions that should be translated via it's center
-- | to actual screen coordinates (see: `points` and `drawTetromino`)
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


-- | constructor function for a `Tetromino`
mkTetromino :: String -> Coord -> Array Coord -> Tetromino
mkTetromino col cen bls = { blocks: bls, color: col, center: cen }


-- | rotates a `Tetromino` at counter-clockwise around it's center
rotateTetromino :: Tetromino -> Tetromino
rotateTetromino tetr =
  tetr { blocks = rotate90at tetr.center <$> tetr.blocks  }


-- | draws a `Tetromino` into a canvas context `ctx` at the point `pt`
drawTetromino :: ∀ e. Context2D -> Point -> Tetromino -> Eff ( canvas :: CANVAS | e ) Unit               
drawTetromino ctx pt tetr = do
    _ <- setStrokeStyle "#000000" ctx
    _ <- setLineWidth 0.1 ctx
    for_ (points pt tetr) (drawBlock ctx tetr.color)
    pure unit


-- | get's all the coordinates a `Tetromino` occupies on screen
-- | by translating the `Tetromino`s coords by `pt`
points :: Point -> Tetromino -> Array Point
points pt tetr =
    map (toPoint <<< translate (fromPoint pt)) tetr.blocks


-- | draws a *block* given by a color `col` and a `Point` `pt` into the canvas
-- | context `ctx`
-- | this functions assumes that the canvas was scaled so that a single
-- | 1pt width and height point will represent the complete block
drawBlock :: ∀ e. Context2D -> String -> Point -> Eff ( canvas :: CANVAS | e ) Unit               
drawBlock ctx col pt = do
    _ <- setFillStyle col ctx
    _ <- strokePath ctx $
    fillPath ctx $ 
    rect ctx
        { x: toNumber (fst pt)
        , y: toNumber (snd pt)
        , w: 1.0
        , h: 1.0
        }
    pure unit

