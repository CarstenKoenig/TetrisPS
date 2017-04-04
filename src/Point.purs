module Point
( Coord (..)
, Point
, point
, rotate90at
, translate
, toPoint
, fromPoint
) where

import Prelude
import Data.Int (floor, toNumber)
import Data.Tuple (snd, fst, Tuple(Tuple))


data Coord =
    Coord Number Number


type Point =
    Tuple Int Int


point :: Int -> Int -> Point
point = Tuple


-- please note: mul is obviosly wrong
instance semiringCoord :: Semiring Coord where
  one = Coord 1.0 1.0
  zero = Coord 0.0 0.0
  add (Coord ax ay) (Coord bx by) = Coord (ax+bx) (ay+by)
  mul (Coord ax ay) (Coord bx by) = Coord (ax*bx) (ay*by)


toPoint :: Coord -> Point
toPoint (Coord x y) = 
  Tuple (floor x) (floor y)


fromPoint :: Point -> Coord
fromPoint p = 
  Coord (toNumber $ fst p) (toNumber $ snd p)


rotate90at :: Coord -> Coord -> Coord
rotate90at (Coord cx cy) = 
  translate (Coord cx cy) <<< rotate90 <<< translate (Coord (-cx) (-cy))


translate :: Coord -> Coord -> Coord 
translate = add


rotate90 :: Coord -> Coord
rotate90 (Coord x y) = 
  Coord (-y) x

