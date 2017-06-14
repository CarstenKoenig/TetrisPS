module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (IntervalId, TIMER, setInterval)
import Control.Monad.Except (runExcept)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.KeyboardEvent (code, eventToKeyboardEvent)
import DOM.Event.Types (EventType(EventType))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import Data.Either (Either(Right))
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(..), fst)
import Game (GameState, GameSettings, moveLeft, moveRight, rotate, drawGame, initializeGame, updateMovingBlock, updateFalling)
import Graphics.Canvas (ScaleTransform, CANVAS, CanvasElement, Context2D, getCanvasHeight, getCanvasWidth, scale, getContext2D, getCanvasElementById)
import JQuery (setScore, showGameOver)
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Signal (foldp, runSignal, (~>))
import Signal.DOM (animationFrame)

type State s = STRef s GameState

updateGame :: forall e s. (GameState -> GameState) -> State s -> Eff (st :: ST s | e) GameState
updateGame update state = 
  modifySTRef state update


updateGame' :: forall e s. (GameState -> Eff (random :: RANDOM, st :: ST s | e) GameState) -> State s -> Eff (st :: ST s, random :: RANDOM | e) GameState
updateGame' update state = do 
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
    let settings = { rows: 20, cols: 12}
    game <- initializeGame settings
    state <- newSTRef game
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    scaling <- calculateScaling settings canvas
    _ <- scale scaling ctx

    initializeInput ctx state
    loopId <- initializeLoop ctx state
    drawGame ctx game



initializeLoop :: forall e s. Context2D -> State s -> Eff (console :: CONSOLE, timer :: TIMER, st :: ST s, canvas :: CANVAS, random :: RANDOM, dom :: DOM | e) IntervalId
initializeLoop ctx state = do
  animFrameSignal <- animationFrame
  let diffSignal = foldp (\ n (Tuple _ l) -> Tuple (abs $ n-l) n) (Tuple 0.0 0.0) animFrameSignal ~> fst
      logSignal = diffSignal ~> logShow
  runSignal logSignal
  setInterval 1500 (loop ctx state)


loop :: forall e s. Context2D -> State s -> Eff (st :: ST s, random :: RANDOM, canvas :: CANVAS, dom :: DOM | e) Unit
loop ctx state = do
  game <- updateGame' updateFalling state
  drawGame ctx game
  showGameOver game.gameOver
  setScore $ show game.score


initializeInput :: forall s e. Context2D -> State s -> Eff ( dom :: DOM, st :: ST s, canvas :: CANVAS, random :: RANDOM | e) Unit
initializeInput ctx state = do
  w <- window
  addEventListener 
    (EventType "keydown")
    (eventListener $ onKeyPress ctx state)
    false
    (windowToEventTarget w)
  pure unit


onKeyPress :: forall eff s. Context2D -> State s -> Event -> Eff ( st :: ST s, random :: RANDOM, canvas :: CANVAS, dom :: DOM | eff) Unit
onKeyPress ctx state event = do

  case map code (runExcept (eventToKeyboardEvent event)) of
    Right "ArrowLeft" -> do
      game <- updateGame (updateMovingBlock moveLeft) state
      drawGame ctx game
    Right "ArrowRight" -> do
      game <- updateGame (updateMovingBlock moveRight) state
      drawGame ctx game
    Right "ArrowDown" -> do
      game <- updateGame' updateFalling state
      drawGame ctx game
    Right "ArrowUp" -> do
      game <- updateGame (updateMovingBlock rotate) state
      drawGame ctx game
    _ -> 
      pure unit


-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: forall e. GameSettings -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling { rows: h, cols: w } canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }
