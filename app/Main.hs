module Main where

import System.IO
import qualified Data.Glome.Vec as V
import qualified Data.List as List
import Control.Monad.State
import Data.Foldable

import Scad
-- TODO: main shouldn't contain rendering logic, only configuration arrays
import Scad.Builders
import Scad.Sandwidge
import GeneralUtils
import Keyboard
import Config
import Parts
import Transformation
import Parts.Switch
import Parts.Envelope
import Parts.RectEnvelope
import Parts.Plate
import qualified Scad.SphereConnectors as Sp

-- We assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"

buildFinalPart :: [[Switch]] -> Maybe Envelope -> Maybe (Double, Double)
               -> String
buildFinalPart switches envelope rectEnvelope =
  renderToScad $ difference [
    union [
            buildPlate switches
          , (buildKeycaps $ concat switches)
          , buildEnvelope switches envelope
          , doRectEnvelope rectEnvelope
          ]
    , buildHoles switches
    ]
  where
    doRectEnvelope Nothing = dummyFigure
    doRectEnvelope (Just (x_len, y_len)) =
      buildRectEnvelope switches
        (computeRectEnvelopeAroundPlate switches x_len y_len 2.5)

mainPlate :: [[Switch]]
mainPlate = fmap (fmap $ transform (Translate $ V.Vec 0 0 (-5))) [
 [ switch (V.vec 0   0    7)  (V.vec (-25) 10    5)
 , switch (V.vec 22  2    4)  (V.vec (-25) 0     5)
 , switch (V.vec 44  2    4)  (V.vec (-25) 0     0)
 , switch (V.vec 66  4    4)  (V.vec (-25) 0     0)
 , switch (V.vec 90  (-2) 6)  (V.vec (-25) (-10) (-15))
 , switch (V.vec 111 (-6) 12) (V.vec (-25) (-20) (-15))
 ],
 [ switch (V.vec 0   23   2)  (V.vec (0)   10    5)
 , switch (V.vec 22  25   0)  (V.vec (0)   0     5)
 , switch (V.vec 44  28   0)  (V.vec (0)   0     0)
 , switch (V.vec 66  30   0)  (V.vec (0)   0     0)
 , switch (V.vec 90  22   2)  (V.vec (0)   (-10) (-15))
 , switch (V.vec 111 18   09) (V.vec (0)   (-20) (-15))
 ],
 [ switch (V.vec 0   46   8)  (V.vec (25)  10    5)
 , switch (V.vec 22  48   6)  (V.vec (25)  0     5)
 , switch (V.vec 44  54   6)  (V.vec (25)  0     0)
 , switch (V.vec 66  56   6)  (V.vec (25)  0     0)
 , switch (V.vec 90  46   8)  (V.vec (25)  (-10) (-15))
 , switch (V.vec 111 42   15) (V.vec (30)  (-23) (-15))
 ]
 ]

-- From left to right from front to back
-- Front and back sides are in same order. The same with left and right.
mainEnvelop :: Envelope
mainEnvelop = Envelope {
    front = [V.vec (-20) (-20) 10]
  , back  = [V.vec (-20) (70) 10, V.vec 140 (70) 10]
  , left  = [V.vec (-20) (-20) 10, head (back mainEnvelop)]
  , right = [V.vec 140 (10) 10, last (back mainEnvelop)]
  , upDirection = V.Vec 0 0 1
  }

mainRectEnvelope :: (Double, Double)
mainRectEnvelope = (140, 85)

thumbPlate :: [[Switch]]
thumbPlate = [
 [ switch (V.vec 0  0  6) (V.vec (-15) (15)  0)
 , switch (V.vec 22 0  3) (V.vec (-15) 0     0)
 , switch (V.vec 44 0  6) (V.vec (-15) (-15) 0)
 ],
 [ switch (V.vec 0  22 3) (V.vec 0     (15)  0)
 , switch (V.vec 22 22 0) (V.vec 0     0     0)
 , switch (V.vec 44 22 3) (V.vec 0     (-15) 0)
 ],
 [ switch (V.vec 0  44 6) (V.vec (15)  (15)  0)
 , switch (V.vec 22 44 3) (V.vec (15)  0     0)
 , switch (V.vec 44 44 6) (V.vec (15)  (-15) 0)
 ]
 ]

singleSocket :: [[Switch]]
singleSocket = [[switch (V.vec 0 0 0) (V.vec 0 0 0)]]

main :: IO ()
main = do withFile "main_plate.scad"  WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing (Just mainRectEnvelope)
          withFile "main_plate2.scad" WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing Nothing
          withFile "thumb_plate.scad" WriteMode $ \h -> hPutStrLn h $  buildFinalPart thumbPlate Nothing Nothing
          withFile "single_socket.scad" WriteMode $ \h -> hPutStrLn h $ buildFinalPart singleSocket Nothing Nothing
