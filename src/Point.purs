module Point
( Coord (..)
, Point
, point
, rotate90at
) where

import Prelude
import Data.Int (floor, toNumber)


data Coord =
    Coord Number Number


type Point =
    { x :: Int, y :: Int }


point :: Int -> Int -> Point
point x y = { x: x, y: y }    


-- please note: mul is obviosly wrong
instance semiringCoord :: Semiring Coord where
  one = Coord 1.0 1.0
  zero = Coord 0.0 0.0
  add (Coord ax ay) (Coord bx by) = Coord (ax+bx) (ay+by)
  mul (Coord ax ay) (Coord bx by) = Coord (ax*bx) (ay*by)


toPoint :: Coord -> Point
toPoint (Coord x y) = 
  { x: floor x, y: floor y }


fromPoint :: Point -> Coord
fromPoint p = 
  Coord (toNumber p.x) (toNumber p.y)


rotate90at :: Coord -> Coord -> Coord
rotate90at (Coord cx cy) = 
  translate (Coord cx cy) <<< rotate90 <<< translate (Coord (-cx) (-cy))


translate :: Coord -> Coord -> Coord 
translate = add


rotate90 :: Coord -> Coord
rotate90 (Coord x y) = 
  Coord (-y) x