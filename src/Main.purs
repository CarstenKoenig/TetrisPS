module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.HTML (window)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst)
import Game (GameSettings, GameState, Message(..), drawGame, initializeGame, update)
import Graphics.Canvas (ScaleTransform, CANVAS, CanvasElement, Context2D, getCanvasHeight, getCanvasWidth, scale, getContext2D, getCanvasElementById)
import JQuery (setScore, showGameOver)
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Signal (foldp, merge, runSignal, (~>))
import Signal.DOM (animationFrame, keyPressed)

type State s = STRef s GameState

updateGame :: forall e s. (GameState -> Eff (random :: RANDOM, st :: ST s | e) GameState) -> State s -> Eff (st :: ST s, random :: RANDOM | e) GameState
updateGame update state = do 
  game <- readSTRef state 
  if game.gameOver 
    then pure game
    else do
      game' <- update game 
      writeSTRef state game'


getGameState :: forall e s. State s -> Eff (st :: ST s | e) GameState 
getGameState state = 
  readSTRef state


main :: forall e s. Eff ( canvas :: CANVAS, st :: ST s, timer :: TIMER, dom :: DOM , console :: CONSOLE, random :: RANDOM | e) Unit
main = void $ unsafePartial do
    let settings = { rows: 20, cols: 12, initialSpeed: Milliseconds 1000.0}
    game <- initializeGame settings
    state <- newSTRef game
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling settings canvas
    _ <- scale scaling ctx

    initializeInput ctx state
    loopId <- initializeLoop ctx state
    drawGame ctx game



initializeLoop :: forall e s. Context2D -> State s -> Eff (console :: CONSOLE, timer :: TIMER, st :: ST s, canvas :: CANVAS, random :: RANDOM, dom :: DOM | e) Unit
initializeLoop ctx state = do
  animFrameSignal <- animationFrame
  leftSignal  <- (\s -> s ~> onDown MoveLeft)  <$> keyPressed 37 
  upSignal    <- (\s -> s ~> onDown Rotate)    <$> keyPressed 38
  rightSignal <- (\s -> s ~> onDown MoveRight) <$> keyPressed 39
  downSignal  <- (\s -> s ~> onDown MoveDown)  <$> keyPressed 40
  let tickSignal   = foldp (\ n (Tuple _ l) -> Tuple (abs $ n-l) n) (Tuple 0.0 0.0) animFrameSignal ~> Ticked <<< Milliseconds <<< fst
      signals      = foldl1 merge (NonEmpty tickSignal [leftSignal, upSignal, rightSignal, downSignal])
      updateSignal = signals ~> upd
  runSignal updateSignal
  where
    onDown s true  = s
    onDown _ false = (Ticked $ Milliseconds 0.0)
    upd msg = do
      game <- updateGame (update msg) state
      view ctx game
      

view :: forall e . Context2D -> GameState -> Eff (canvas :: CANVAS, dom :: DOM | e) Unit
view ctx game = do
  drawGame ctx game
  showGameOver game.gameOver
  setScore $ show game.score


initializeInput :: forall s e. Context2D -> State s -> Eff ( dom :: DOM, st :: ST s, canvas :: CANVAS, random :: RANDOM | e) Unit
initializeInput ctx state = do
  w <- window
  pure unit


-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: forall e. GameSettings -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling { rows: h, cols: w } canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }
