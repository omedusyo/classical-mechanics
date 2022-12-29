module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import DOM as DOM
import VDOM.Render as Render
import VDOM.Examples (ex1, updateCounter, initialCounterModel, viewCounter)
import Effect.Extra.Timer as Timer
import Duration as Duration
import Data.Number ( pi )
import ClassicalMechanics.Pendulum as Pendulum
import Store as Store
import Event as Event

import Canvas as Canvas
import CanvasGeometry.Element as Element 


canvasConfig0 :: Canvas.CanvasConfig
canvasConfig0 = { width: 1000.0, height: 900.0 }

cartesianToCanvasCoord :: Canvas.CanvasConfig -> { x :: Number, y :: Number } -> { canvasX :: Number, canvasY :: Number }
cartesianToCanvasCoord canvas { x, y } =
  -- 1 meter == 10 pixels
  let scale = 10.0 in
  { canvasX: canvas.width/2.0 + scale * x, canvasY: canvas.height/2.0 - scale * y }

main :: Effect Unit
main = do
  -- div <- DOM.make "div"
  -- txt <- DOM.text "hello, world"
  -- DOM.appendChild div txt
  -- root <- DOM.root
  -- DOM.appendChild root div

  -- DOM.setAttribute div "class" "foo"

  -- -- ===counter===
  -- _ <- Render.fromComponent
  --   (\msg model -> pure (updateCounter msg model))
  --   (pure initialCounterModel)
  --   viewCounter
  --   root

  -- Timer.intervalLoop (Duration.fromMilliseconds 16) 0 (\state ->
  --   if state < 20 then do
  --     log (show state)
  --     pure (state + 1) 
  --   else
  --     pure state
  -- )


  -- ===canvas===
  root <- DOM.root
  { canvasRef, canvasDOM } <- DOM.makeCanvas canvasConfig0
  DOM.appendChild root canvasDOM
  DOM.setAttribute canvasDOM "style" "border: 1px solid black"

  -- rectangle <- Canvas.makePath2D
  -- rectangle # Canvas.rect 200.0 300.0 150.0 89.0
  -- canvasRef # Canvas.stroke rectangle

  -- circle <- Canvas.makePath2D
  -- circle # Canvas.arc 500.0 400.0 50.0 0.0 (2.0*pi)
  -- canvasRef # Canvas.fill circle

  -- path <- Canvas.makePath2D
  -- path # Canvas.moveTo 50.0 50.0
  -- path # Canvas.lineTo 100.0 50.0
  -- path # Canvas.lineTo 100.0 100.0
  -- path # Canvas.lineTo 50.0 100.0
  -- path # Canvas.lineTo 25.0 75.0
  -- path # Canvas.lineTo 50.0 50.0
  -- canvasRef # Canvas.fill path

  let pendulumState0 = { time: 0.0, position: 0.0 * 2.0*pi, velocity: 0.0 }
  let pendulumState1 = { time: 0.0, position: 0.15 * 2.0*pi, velocity: 0.0 }
  let pendulumState2 = { time: 0.0, position: 0.15 * 2.0*pi, velocity: -2.0 }
  let pendulumState3 = { time: 0.0, position: 0.15 * 2.0*pi, velocity: 6.0 }
  let pendulumState4 = { time: 0.0, position: -0.25 * 2.0*pi, velocity: 0.0 }
  let pendulumState5 = { time: 0.0, position: -0.26 * 2.0*pi, velocity: 0.0 }
  let pendulumState6 = { time: 0.0, position: -0.25 * 2.0*pi, velocity: 20.0 }

  let pendulumConfig = Pendulum.config0 { radius: 25.0 }
  let initState =
        { shouldRequestAnimationFrame: true
        , timestamp: 0.0
        , pendulumState: pendulumState0
        }


  let requestAnimationFrame store = 
        Event.requestAnimationFrame
          (\timestamp -> do
            state <- store # Store.getModel
            store # Store.send { shouldRequestAnimationFrame: state.shouldRequestAnimationFrame, timestamp: timestamp, pendulumMsg: Pendulum.NewFrameRequested (timestamp - state.timestamp) }
          )

  pendulumStore <-
    Store.make
      (\pendulumStore msg state -> do
        newPendulumState <-
          Pendulum.update pendulumConfig msg.pendulumMsg state.pendulumState

        canvasRef # Canvas.clear canvasConfig0
        Element.render
          canvasConfig0
          canvasRef
          (Pendulum.view pendulumConfig newPendulumState)


        if msg.shouldRequestAnimationFrame then do
          requestAnimationFrame pendulumStore
        else
          pure unit

        pure (state { timestamp = msg.timestamp, pendulumState = newPendulumState })
      )
      initState
  
  requestAnimationFrame pendulumStore

  -- ===Buttons===
  button0 <- DOM.make "button"
  DOM.appendChild root button0
  DOM.setTextContent button0 "State 0"

  button1 <- DOM.make "button"
  DOM.appendChild root button1
  DOM.setTextContent button1 "State 1"

  button2 <- DOM.make "button"
  DOM.appendChild root button2
  DOM.setTextContent button2 "State 2"

  button3 <- DOM.make "button"
  DOM.appendChild root button3
  DOM.setTextContent button3 "State 3"

  button4 <- DOM.make "button"
  DOM.appendChild root button4
  DOM.setTextContent button4 "State 4"

  button5 <- DOM.make "button"
  DOM.appendChild root button5
  DOM.setTextContent button5 "State 5"

  button6 <- DOM.make "button"
  DOM.appendChild root button6
  DOM.setTextContent button6 "State 6"

  let attachResetStateOnClick button pendulumState =
        DOM.attachClickEvent button (do
            state <- pendulumStore # Store.getModel
            pendulumStore # Store.send { shouldRequestAnimationFrame: false, timestamp: state.timestamp, pendulumMsg: Pendulum.ResetOfStateWasRequested pendulumState }
          )

  attachResetStateOnClick button0 pendulumState0
  attachResetStateOnClick button1 pendulumState1
  attachResetStateOnClick button2 pendulumState2
  attachResetStateOnClick button3 pendulumState3
  attachResetStateOnClick button4 pendulumState4
  attachResetStateOnClick button5 pendulumState5
  attachResetStateOnClick button6 pendulumState6

  pure unit

clearWithOpacity :: Canvas.CanvasConfig -> Canvas.CanvasRef -> Effect Unit
clearWithOpacity canvas canvasRef = do
  rectangle <- Canvas.makePath2D
  rectangle # Canvas.rect 0.0 0.0 canvas.width canvas.height
  canvasRef # Canvas.push (do
    canvasRef # Canvas.setFillStyle "#FFFFFF"
    canvasRef # Canvas.setGlobalAlpha 0.10
    canvasRef # Canvas.fill rectangle
  )
