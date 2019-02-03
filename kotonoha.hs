{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.Graphics.Canvas
import Haste.Foreign

textfile :: String
textfile = "kotonoha.txt"

startX :: Double
startX = 400

startY :: Double
startY = 100

main :: IO ()
main = do
    Just can <- getCanvasById "canvas"
    tx <- readFromFile textfile  
    view can tx

view :: Canvas -> String -> IO ()
view can tx = do
    render can $ do
      lineWidth 2.0 $ do
        color (RGB 255 255 255) $ stroke $ rect (0,0) (450,700) 
      mapM_ (color (RGBA 255 255 255 0.8) . font "24px oshide") $ letterList tx 
      return ()            

letterList :: String -> [Picture ()]
letterList ts = do
    zipWith text (zip [a*b | a <- [startX,(startX-40)..], b <- take 14 (repeat 1)]
                        (map (+startY) $ cycle [30,60..(30*14)])) (map (\c -> c : []) ts)
    
readFromFile :: String -> IO String
readFromFile = ffi "(function(fname){var text=null; var ajax=new XMLHttpRequest(); ajax.open('GET',fname,false); ajax.onreadystatechange = function(){if(ajax.readyState==4){if(ajax.status==200 || ajax.status==0){text=ajax.responseText;};};}; ajax.send(null); return text;})"

