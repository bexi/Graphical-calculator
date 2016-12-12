import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages
import Expr

canWidth  = 300
canHeight = 300
scale     = 0.04

-- reads the expression from the given input element
-- and draws the graph on the given canvas
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw = undefined

-- will calculate all the points of the graph in terms of pixels
points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (x,y) = [(pixel, realToPix (eval expr pixel)) | pixel <- [0..300]]
    where
          -- converts a pixel x-coordinate to a real x-coordinate
          pixToReal :: Double -> Double
          pixToReal x = x/25-6

          -- converts a real y-coordinate to a pixel y-coordinate
          realToPix :: Double -> Double
          realToPix y = (y-6)*(-25)

-- zoom in and out

-- differentiate

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- fromElem canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13
