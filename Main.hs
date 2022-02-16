{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Char
import qualified Data.Text as T
import Diagrams.Backend.SVG
import Diagrams.Core.Types
import Diagrams.Core.Compile
import Diagrams.Prelude
import Diagrams.SVG.ReadSVG
import Diagrams.TwoD.Input
import Diagrams.TwoD.Size (dims2D)
import Filesystem.Path.CurrentOS (decodeString)
import Lucid.Svg
import Prelude hiding (FilePath)
import System.Directory
import System.FilePath

main = do
  resourceFiles <- getDirectoryContents "."
  let svgs = filter (\xs -> xs /= "."  && 
                            xs /= ".." && 
                            ( (lastN 4 xs) == ".svg" || (lastN 4 xs) == ".png" || (lastN 4 xs) == ".jpg" )) resourceFiles
  putStr $ show svgs
  images <- mapM loadImageEmbedded svgs
  let files = map ("out-" ++) svgs
  zipWithM_ (\f i -> renderSVG f (dims2D 400 400) i) files (map img images)

img :: Either String (Diagram SVG) -> Diagram SVG
img im = case im of Left err -> mempty
                    Right i -> i

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs
