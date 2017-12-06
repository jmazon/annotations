module Main where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

import Text.XML.Light

import Data.List
import Control.Monad

bsearch valid = go 0 where
  go a s = do
    v <- valid s
    if v then go s (2*s) else go' a s
  go' a b | b - a < 0.1 = valid a *> pure a
          | otherwise = do
    let m = (a + b) / 2
    v <- valid m
    if v then go' m b else go' a m

simpleName n = blank_name { qName = n }

main :: IO ()
main = do
  xml <- parseXML <$> readFile "annotations.xml"
  let annotations = sortOn annStart $ map processAnnotation $
                    concatMap (findElements (simpleName "annotation"))
                              (onlyElems xml)
  -- print (head annotations)

  let width = 640
      height = 480

  -- let 
      
  forM_ (zip [0..] annotations) $ \(i,ann) -> do
    let w = annW ann * width / 100
        h = annH ann * height / 100
    print (w,h)
    makeAnnotationPng w h (annText ann) ("ann" ++ show i ++ ".png")

  putStr $ unlines $ map annText annotations
  

data Ann = Ann { annText :: String
               , annStart :: String
               , annEnd :: String
               , annX :: Double
               , annY :: Double
               , annW :: Double
               , annH :: Double }

processAnnotation ann = Ann text start end (read x) (read y) (read w) (read h)
 where
  Just text = strContent <$> findChild (simpleName "TEXT") ann
  [r1,r2] = findElements (simpleName "anchoredRegion") ann
  Just start = findAttr (simpleName "t") r1
  Just end = findAttr (simpleName "t") r2
  Just x = findAttr (simpleName "x") r1
  Just y = findAttr (simpleName "y") r1
  Just w = findAttr (simpleName "w") r1
  Just h = findAttr (simpleName "h") r1

makeAnnotationPng tWidth tHeight text pngName = do
  p <- cairoCreateContext Nothing
  txt <- layoutText p text
  layoutSetWidth txt (Just $ tWidth-16)
  layoutSetWrap txt WrapPartialWords
  fd <- fontDescriptionNew

  let valid s = do
        fontDescriptionSetSize fd s
        layoutSetFontDescription txt (Just fd)
        (_,r@(PangoRectangle _ _ _ h)) <- layoutGetExtents txt
        -- putStrLn $ show s ++ " " ++ show r
        return (h <= tHeight - 16)
  bsearch valid 16
          
  withImageSurface FormatRGB24 (round tWidth + 16) (round tHeight + 16) $
   \s -> do
    renderWith s $ do
      setSourceRGB (254/255) (164/255) 0
      paint
      withLinearPattern 0 0 8 0 $ \p -> do
        patternAddColorStopRGB p 0 1 1 1
        patternAddColorStopRGB p 1 (254/255) (164/255) 0
        setSource p
        moveTo 0 0
        lineTo 8 8
        lineTo 8 (tHeight + 8)
        lineTo 0 (tHeight + 16)
        closePath
        fill
      withLinearPattern 0 0 0 8 $ \p -> do
        patternAddColorStopRGB p 0 1 1 1
        patternAddColorStopRGB p 1 (254/255) (164/255) 0
        setSource p
        moveTo 0 0
        lineTo 8 8
        lineTo (tWidth+8) 8
        lineTo (tWidth+16) 0
        closePath
        fill
      withLinearPattern (tWidth+16) 0 (tWidth+8) 0 $ \p -> do
        patternAddColorStopRGB p 0 0 0 0
        patternAddColorStopRGB p 1 (254/255) (164/255) 0
        setSource p
        moveTo (tWidth+16) 0
        lineTo (tWidth+8) 8
        lineTo (tWidth+8) (tHeight+8)
        lineTo (tWidth+16) (tHeight+16)
        closePath
        fill
      withLinearPattern 0 (tHeight+16) 0 (tHeight+8) $ \p -> do
        patternAddColorStopRGB p 0 0 0 0
        patternAddColorStopRGB p 1 (254/255) (164/255) 0
        setSource p
        moveTo 0 (tHeight+16)
        lineTo 8 (tHeight+8)
        lineTo (tWidth+8) (tHeight+8)
        lineTo (tWidth+16) (tHeight+16)
        closePath
        fill

      moveTo 16 16
      (_,PangoRectangle _ _ w h) <- liftIO $ layoutGetExtents txt
      relMoveTo ((tWidth - 16 - w) / 2) ((tHeight - 16 - h) / 2)
      setSourceRGB 0 0 0
      showLayout txt

    surfaceWriteToPNG s pngName
  putStrLn $ "Wrote " ++ pngName
