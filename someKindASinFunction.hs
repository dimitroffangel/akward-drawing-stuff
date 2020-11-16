import Control.DeepSeq (deepseq)
import Control.Concurrent (threadDelay)
import System.Process 

defaultColourMap = " .^*#0@"

normalizeGrid grid =
  let max = maximum $ map maximum grid
      min = minimum $ map minimum grid
   in [[(p - min) / (max - min) | p <- row] | row <- grid]

shadePixelEpsilon = 0.0001

shadePixel :: Double -> [a] -> a
shadePixel n colourMap = 
    let cml = fromIntegral $ length colourMap
        s = floor (n * (cml - shadePixelEpsilon))
        in colourMap !! s

grid f step =
    [ 
        [ 
            f x y | x <- [-1, -1 + step .. 1]
        ] 
        | y <- [-1, -1 + step .. 1]
    ]

shadeGrid grid colourMap = 
    [ 
        [ 
            shadePixel pixel colourMap | pixel <- row
        ] 
        | row <- normalizeGrid grid
    ]

render canvas = 
    let newLineCanvas = [concat [[p, p, p] | p <- row] ++ "\n" | row <- canvas]
        in concat newLineCanvas

func t x y = sin (x * y * 10 + t / 10)

printRenderer t = 
    let canvas = render $ shadeGrid (grid (func t) 0.05) defaultColourMap
        in canvas `deepseq` putStrLn canvas

saveRenderer t = 
    writeFile "./render.txt" $ render $ shadeGrid (grid (func t) 0.05) defaultColourMap

clear = system "cls"

loop t = do
    clear
    printRenderer t
    threadDelay 5000
    loop $ t + 1

main = loop 0