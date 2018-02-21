module Main where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.List               as List
import           Data.Maybe
import           Data.Monoid
import           Demo.Body               (bodyDemo)
import           Keyboard
import           Keyboard.Transformation
import           System.IO

buildFinalPart :: [[Switch]] -> Maybe Envelope -> Maybe (Double, Double)
               -> ScadProgram
buildFinalPart switches envelope rectEnvelope = hollowFigureToScad $
  buildPlate switches
  <> buildKeycaps switches
  <> maybeFigure (buildEnvelope switches <$> envelope)
  <> maybeFigure (buildRectEnvelope switches . compureRectCoords <$> rectEnvelope)
  where
    maybeFigure = fromMaybe mempty
    compureRectCoords (x_len, y_len) = (computeRectEnvelopeAroundPlate switches x_len y_len 2.5)

mainPlate :: [[Switch]]
mainPlate = transform (Translate $ vec 0 0 (-5)) [
 [ switch (vec (0)    (-4) (6))  (vec (-30) (0) (0))
 , switch (vec (19.5) (-2) (6))  (vec (-30) (0) (0))
 , switch (vec (39)   (0)  (4))  (vec (-30) (0) (0))
 , switch (vec (58.5) (1)  (1))  (vec (-30) (0) (0))
 , switch (vec (78)   (-2) (4))  (vec (-30) (0) (0))
 , switch (vec (97.5) (-4) (4))  (vec (-30) (0) (0))
 ],
 [ switch (vec (0)    (18) (0))  (vec (0)   (0) (0))
 , switch (vec (19.5) (20) (0))  (vec (0)   (0) (0))
 , switch (vec (39)   (22) (-2)) (vec (0)   (0) (0))
 , switch (vec (58.5) (23) (-5)) (vec (0)   (0) (0))
 , switch (vec (78)   (20) (-2)) (vec (0)   (0) (0))
 , switch (vec (97.5) (18) (-2)) (vec (0)   (0) (0))
 ],
 [ switch (vec (0)    (40) (6))  (vec (30)  (0) (0))
 , switch (vec (19.5) (42) (6))  (vec (30)  (0) (0))
 , switch (vec (39)   (44) (4))  (vec (30)  (0) (0))
 , switch (vec (58.5) (45) (1))  (vec (30)  (0) (0))
 , switch (vec (78)   (42) (4))  (vec (30)  (0) (0))
 , switch (vec (97.5) (40) (4))  (vec (30)  (0) (0))
 ]
 ]

-- From left to right from front to back
-- Front and back sides are in same order. The same with left and right.
mainEnvelop :: Envelope
mainEnvelop = Envelope {
    front = [vec (-20) (-20) 10]
  , back  = [vec (-20) (70) 10, vec 140 (70) 10]
  , left  = [vec (-20) (-20) 10, head (back mainEnvelop)]
  , right = [vec 140 (10) 10, last (back mainEnvelop)]
  , upDirection = vec 0 0 1
  }

mainRectEnvelope :: (Double, Double)
mainRectEnvelope = (130, 70)

thumbPlate :: [[Switch]]
thumbPlate = [
 [ switch (vec 0    0    (1))  (vec (-15) (15)  0)
 , switch (vec 21.5 0    (-2)) (vec (-15) 0     0)
 , switch (vec 43   0    (1))  (vec (-15) (-15) 0)
 ],
 [ switch (vec 0    21.5 (-2)) (vec 0     (15)  0)
 , switch (vec 21.5 21.5 (-5)) (vec 0     0     0)
 , switch (vec 43   21.5 (-2)) (vec 0     (-15) 0)
 ],
 [ switch (vec 0    43   (1))  (vec (15)  (15)  0)
 , switch (vec 21.5 43   (-2)) (vec (15)  0     0)
 , switch (vec 43   43   (1))  (vec (15)  (-15) 0)
 ]
 ]

thumbRectEnvelope :: (Double, Double)
thumbRectEnvelope = (65, 65)

singleSocket :: [[Switch]]
singleSocket = [[switch (vec 0 0 0) (vec 0 0 0)]]

writeFigureToFile :: FilePath -> ScadProgram -> IO ()
writeFigureToFile fileName figure =
  withFile fileName WriteMode $ \h -> hPutStrLn h (renderToScad figure)

writeFinalPartToFile ::
  (FilePath, Plate, Maybe Envelope, Maybe (Double, Double))
  -> IO ()
writeFinalPartToFile (fileName, plate, envelope, rectEnvelope) =
  writeFigureToFile fileName (buildFinalPart plate envelope rectEnvelope)

main :: IO ()
main = do
  mapM_ writeFinalPartToFile [
      ("main_plate.scad", mainPlate, Nothing, Just mainRectEnvelope)
    , ("main_plate2.scad", mainPlate, Nothing, Nothing)
    , ("thumb_plate.scad", thumbPlate, Nothing, Just thumbRectEnvelope)
    , ("single_socket.scad", singleSocket, Nothing, Nothing)
    ]
  writeFigureToFile "body_demo.scad" (bodyDemo mainPlate thumbPlate)
