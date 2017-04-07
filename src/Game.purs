module Game 
    ( GameState
    , GameSettings
    , Color
    , Score
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
import Data.List as List
import Data.Map as Map
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Foldable (for_, foldl)
import Data.List (List, filter)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Ordering (invert)
import Data.Traversable (all)
import Data.Tuple (snd, Tuple(Tuple), fst)
import Graphics.Canvas (setLineWidth, setStrokeStyle, CANVAS, Context2D, fillRect, setFillStyle)
import Point (point, Point)
import Tetrominos (FallingTetromino, drawBlock, drawTetromino, rotateTetromino, points, randomTetromino)


type Color = String
type Score = Int


type GameSettings = 
    { rows :: Int
    , cols :: Int
    }

type GameState =
    { fallingTetromino :: FallingTetromino
    , occupied :: Map Point Color
    , settings :: GameSettings
    , score :: Score
    , gameOver :: Boolean 
    }



initializeGame :: forall e. GameSettings -> Eff (random :: RANDOM | e) GameState
initializeGame sets = do
    ftetr <- randomTetromino
    pure $ 
        { fallingTetromino: ftetr
        , occupied: (Map.empty :: Map Point Color)
        , settings: sets 
        , score: 0
        , gameOver: false
        }


scorePerRows :: Int -> Score 
scorePerRows n = 5 * n * (n+1)


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
            initNewFalling $ stopFalling gamestate
            

stopFalling :: GameState -> GameState
stopFalling gs =
    let occ' = insertOccupiedTetromino gs.fallingTetromino gs.occupied
        scoreAndOcc = popFullyOccupiedRows gs.settings.rows gs.settings.cols occ'
    in gs { occupied = snd scoreAndOcc
          , score = gs.score + fst scoreAndOcc }


insertOccupiedTetromino :: FallingTetromino -> Map Point Color -> Map Point Color 
insertOccupiedTetromino ftetr map = 
    foldl (flip ins) map $ points ftetr.coord ftetr.tetromino
    where
        ins pt = 
            Map.insert pt ftetr.tetromino.color


initNewFalling :: forall e. GameState -> Eff (random :: RANDOM | e) GameState
initNewFalling gamestate = do
    ftetr <- randomTetromino
    let gamestate' = gamestate { fallingTetromino = ftetr }
    pure $ if not (isValid gamestate' ftetr) 
        then gamestate { gameOver = true }
        else gamestate'


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
  drawOccupied ctx gamestate.occupied
  if not gamestate.gameOver 
    then do
        let ftetr = gamestate.fallingTetromino
        drawFalling ctx ftetr
    else
        pure unit


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


drawOccupied :: forall e. Context2D -> Map Point Color -> Eff ( canvas :: CANVAS | e ) Unit               
drawOccupied ctx occ = do
    let blocks = Map.toList occ
    setStrokeStyle "#000000" ctx
    setLineWidth 0.1 ctx
    for_ blocks (\ tpl -> drawBlock ctx (snd tpl) (fst tpl))
    pure unit


popFullyOccupiedRows :: Int -> Int -> Map Point Color -> Tuple Score (Map Point Color)
popFullyOccupiedRows rows cols occ = 
    Tuple score $ foldl pop occ rs 
    where
        score = scorePerRows $ List.length fullRows
        fullRows = fullyOccupiedRows rows cols occ
        pop m r = popRow r m 
        rs = List.zipWith (+) (List.range 0 rows) fullRows


fullyOccupiedRows :: Int -> Int -> Map Point Color -> List Int 
fullyOccupiedRows rows cols occ = 
    List.filter (isFullyOccupied cols occ) $ List.reverse $ List.range 0 (rows - 1)


isFullyOccupied :: Int -> Map Point Color -> Int -> Boolean 
isFullyOccupied cols occ row = 
    List.length keys == cols 
    where 
        keys = filter (\pt -> snd pt == row) $ Map.keys occ


popRow :: Int -> Map Point Color -> Map Point Color 
popRow row = 
    mapDown row <<< removeRow row


removeRow :: Int -> Map Point Color -> Map Point Color
removeRow row occ =
    foldl rem occ keys 
    where 
        rem m pt = Map.delete pt m 
        keys = filter (\pt -> snd pt == row) $ Map.keys occ


mapDown :: Int -> Map Point Color -> Map Point Color
mapDown above occ = 
    foldl mapD occ keys 
    where 
        mapD m pt = 
            let pt' = point (fst pt) (snd pt + 1) 
            in case Map.pop pt m of 
                Just tpl -> Map.insert pt' (fst tpl) (snd tpl)
                Nothing -> m
        keys = List.sortBy cmpSnd $ filter (\pt -> snd pt < above) $ Map.keys occ
        cmpSnd ptA ptB = invert $ compare (snd ptA) (snd ptB)

