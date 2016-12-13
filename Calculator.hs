import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe
import Parsing

import Pages
import Expr

canWidth  = 300
canHeight = 300
scale     = 0.04

-- reads the expression from the given input element
-- and draws the graph on the given canvas
readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw e s c = do exValue <- getProp e "value"
                       scaleValue <- getProp s "value"
                       let ex = fromJust (readExpr exValue)
                       let scale' = read scaleValue
                       let pts = points ex (Main.scale*scale') (canWidth, canHeight)
                       let shp = path pts
                       let pic = stroke shp
                       render c pic

--readDiffAndDraw input inputScale can
readDiffAndDraw :: Elem -> Elem -> Canvas -> IO ()
readDiffAndDraw e s c = do exValue <- getProp e "value"
                           scaleValue <- getProp s "value"
                           let ex = differentiate (fromJust (readExpr exValue))
                           let scale' = read scaleValue
                           let pts = points ex (Main.scale*scale') (canWidth, canHeight)
                           let shp = path pts
                           let pic = stroke shp
                           set e [ prop "value" =: (show ex) ]
                           render c pic

-- will calculate all the points of the graph in terms of pixels
points :: Expr -> Double -> (Int,Int) -> [Point]
points expr s (w,h) = [(pixel, realToPix (eval expr (pixToReal pixel))) | pixel <- map fromIntegral [0..w]]
    where
          -- converts a pixel x-coordinate to a real x-coordinate
          pixToReal :: Double -> Double
          pixToReal x = x*s - ((fromIntegral w) * s) / 2

          -- converts a real y-coordinate to a pixel y-coordinate
          realToPix :: Double -> Double
          realToPix y = (y - ((fromIntegral h) * s) / 2) / ((-1) * s)

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight    -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="   -- The text "f(x)="
    input   <- mkInput 20 "x"                 -- The formula input
    scaleText    <- mkHTML "Scale: "          -- The text "f(x)="
    inputScale   <- mkInput 20 "1"            -- The formula input
    draw         <- mkButton "Draw graph"     -- The draw button
    diff         <- mkButton "Differentiate"  -- The diff button

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    scaleRow <- mkDiv
    row scaleRow [scaleText,inputScale]
    column documentBody [canvas,formula, scaleRow, draw, diff]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- fromElem canvas
    onEvent draw  Click $ \_    -> readAndDraw input inputScale can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input inputScale can
    -- "Enter" key has code 13
    onEvent diff Click $ \_ -> readDiffAndDraw input inputScale can
    onEvent inputScale KeyUp $ \code -> when (code==13) $ readAndDraw input inputScale can
