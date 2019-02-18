{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.Graphics.Canvas
import Haste.Events (onEvent,BasicEvent(..))
import Haste.DOM (elemOf)
import Haste.Foreign

textfile :: String
textfile = "kotonoha.txt"

imgfile :: URL 
imgfile = "chara.png"

startX :: Double
startX = 410

startY :: Double
startY = 100

main :: IO ()
main = do
    Just can <- getCanvasById "canvas"
    ch <- loadBitmap imgfile
    tx <- readFromFile textfile  
    elemOf ch `onEvent` Load $ \_ -> do
      view can ch tx
    return () 

view :: Canvas -> Bitmap -> String -> IO ()
view can ch tx = do
    render can $ do
      scale (2, 2) $ do
        draw ch (20,5)
      lineWidth 2.0 $ do
        color (RGB 255 255 255) $ stroke $ rect (5,80) (450,520) 
      mapM_ (color (RGBA 255 255 255 0.8) . font "24px oshide") $ letterList tx 
    return ()            

letterList :: String -> [Picture ()]
letterList ts = do
    zipWith text (zip [a*b | a <- [startX,(startX-40)..], b <- take 14 (repeat 1)]
                        (map (+startY) $ cycle [30,60..(30*14)])) (map (\c -> c : []) ts)
    
readFromFile :: String -> IO String
readFromFile = ffi "(function(fname){var text=null; var ajax=new XMLHttpRequest(); ajax.open('GET',fname,false); ajax.onreadystatechange = function(){if(ajax.readyState==4){if(ajax.status==200 || ajax.status==0){text=ajax.responseText;};};}; ajax.send(null); return text;})"

