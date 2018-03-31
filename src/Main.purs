module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst)
import Game (GameSettings, GameState, Message(..))
import Game as Game
import Graphics.Canvas (ScaleTransform, CANVAS, CanvasElement, Context2D, getCanvasHeight, getCanvasWidth, scale, getContext2D, getCanvasElementById)
import JQuery (setScore, showGameOver)
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Signal (foldp, merge, runSignal, (~>))
import Signal.DOM (animationFrame, keyPressed)
import SignalExt (foldEff)


-- | this is the entry point into the app
-- | we setup the canvas, settings and start the game loop
main :: ∀ e. Eff ( canvas :: CANVAS, timer :: TIMER, dom :: DOM, random :: RANDOM | e) Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    let settings = { rows: 20, cols: 12, initialSpeed: Milliseconds 1000.0}
    scaling <- calculateScaling settings canvas
    _ <- scale scaling ctx

    game <- Game.initialize settings
    Game.draw ctx game
    runGameLoop ctx game



-- | we are using `Signal` to map `animationFrame` and keyboard signals into
-- | a `GameState`-signal using `Game.update`
-- | finally we run this signal using `view`
runGameLoop :: ∀ e. Context2D -> GameState -> Eff (timer :: TIMER, canvas :: CANVAS, random :: RANDOM, dom :: DOM | e) Unit
runGameLoop ctx initialGame = do
  gameSignal <- mkGameSignal
  runSignal $ view <$> gameSignal

  where
    view game = do
      Game.draw ctx game
      showGameOver game.gameOver
      setScore $ show game.score

    mkGameSignal = do
      sigs <- signals
      foldEff Game.update initialGame sigs

    signals = do
      leftSignal  <- (\s -> s ~> onDown MoveLeft)  <$> keyPressed 37 
      upSignal    <- (\s -> s ~> onDown Rotate)    <$> keyPressed 38
      rightSignal <- (\s -> s ~> onDown MoveRight) <$> keyPressed 39
      downSignal  <- (\s -> s ~> onDown MoveDown)  <$> keyPressed 40
      tickSignal  <- mkTickSignal
      pure $ foldl1 merge (NonEmpty tickSignal [leftSignal, upSignal, rightSignal, downSignal])

    mkTickSignal = do
      animFrameSignal <- animationFrame
      pure $ foldp (\ n (Tuple _ l) -> Tuple (abs $ n-l) n) (Tuple 0.0 0.0) animFrameSignal 
        ~> Ticked <<< Milliseconds <<< fst

    onDown msg true  = msg
    onDown _   false = Ticked $ Milliseconds 0.0



-- calculates a ScaleTransform so we can use 1pixel per block
calculateScaling :: ∀ e. GameSettings -> CanvasElement          
        -> Eff ( canvas :: CANVAS | e) ScaleTransform               
calculateScaling { rows: h, cols: w } canvas = do
  cW <- getCanvasWidth canvas
  cH <- getCanvasHeight canvas
  pure { scaleX: cW / (toNumber w), scaleY: cH / (toNumber h) }