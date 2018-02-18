module Main where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Glome.Vec        as V
import qualified Data.List             as List
import           System.IO

import           Scad
-- TODO: main shouldn't contain rendering logic, only configuration arrays
import           Config
import           GeneralUtils
import           Keyboard
import           Parts
import           Parts.Envelope
import           Parts.Plate
import           Parts.RectEnvelope
import           Parts.Switch
import           Scad.Builders
import           Scad.Sandwidge
import qualified Scad.SphereConnectors as Sp
import           Transformation

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
    , doRectEnvelopeHole rectEnvelope
    ]
  where
    env x_len y_len = (computeRectEnvelopeAroundPlate switches x_len y_len 2.5)
    doRectEnvelopeHole Nothing = dummyFigure
    doRectEnvelopeHole (Just (x_len, y_len)) =
      rectEnvelopeHole (env x_len y_len)
    doRectEnvelope Nothing = dummyFigure
    doRectEnvelope (Just (x_len, y_len)) =
      buildRectEnvelope switches (env x_len y_len)

mainPlate :: [[Switch]]
mainPlate = fmap (fmap $ transform (Translate $ V.Vec 0 0 (-5))) [
 [ switch (V.vec (0)    (-4) (6))  (V.vec (-30) (0) (0))
 , switch (V.vec (19.5) (-2) (6))  (V.vec (-30) (0) (0))
 , switch (V.vec (39)   (0)  (4))  (V.vec (-30) (0) (0))
 , switch (V.vec (58.5) (1)  (1))  (V.vec (-30) (0) (0))
 , switch (V.vec (78)   (-2) (4))  (V.vec (-30) (0) (0))
 , switch (V.vec (97.5) (-4) (4))  (V.vec (-30) (0) (0))
 ],
 [ switch (V.vec (0)    (18) (0))  (V.vec (0)   (0) (0))
 , switch (V.vec (19.5) (20) (0))  (V.vec (0)   (0) (0))
 , switch (V.vec (39)   (22) (-2)) (V.vec (0)   (0) (0))
 , switch (V.vec (58.5) (23) (-5)) (V.vec (0)   (0) (0))
 , switch (V.vec (78)   (20) (-2)) (V.vec (0)   (0) (0))
 , switch (V.vec (97.5) (18) (-2)) (V.vec (0)   (0) (0))
 ],
 [ switch (V.vec (0)    (40) (6))  (V.vec (30)  (0) (0))
 , switch (V.vec (19.5) (42) (6))  (V.vec (30)  (0) (0))
 , switch (V.vec (39)   (44) (4))  (V.vec (30)  (0) (0))
 , switch (V.vec (58.5) (45) (1))  (V.vec (30)  (0) (0))
 , switch (V.vec (78)   (42) (4))  (V.vec (30)  (0) (0))
 , switch (V.vec (97.5) (40) (4))  (V.vec (30)  (0) (0))
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
mainRectEnvelope = (130, 70)

thumbPlate :: [[Switch]]
thumbPlate = [
 [ switch (V.vec 0    0    (1))  (V.vec (-15) (15)  0)
 , switch (V.vec 21.5 0    (-2)) (V.vec (-15) 0     0)
 , switch (V.vec 43   0    (1))  (V.vec (-15) (-15) 0)
 ],
 [ switch (V.vec 0    21.5 (-2)) (V.vec 0     (15)  0)
 , switch (V.vec 21.5 21.5 (-5)) (V.vec 0     0     0)
 , switch (V.vec 43   21.5 (-2)) (V.vec 0     (-15) 0)
 ],
 [ switch (V.vec 0    43   (1))  (V.vec (15)  (15)  0)
 , switch (V.vec 21.5 43   (-2)) (V.vec (15)  0     0)
 , switch (V.vec 43   43   (1))  (V.vec (15)  (-15) 0)
 ]
 ]

thumbRectEnvelope :: (Double, Double)
thumbRectEnvelope = (65, 65)

singleSocket :: [[Switch]]
singleSocket = [[switch (V.vec 0 0 0) (V.vec 0 0 0)]]

main :: IO ()
main = do withFile "main_plate.scad"  WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing (Just mainRectEnvelope)
          withFile "main_plate2.scad" WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing Nothing
          withFile "thumb_plate.scad" WriteMode $ \h -> hPutStrLn h $  buildFinalPart thumbPlate Nothing (Just thumbRectEnvelope)
          withFile "single_socket.scad" WriteMode $ \h -> hPutStrLn h $ buildFinalPart singleSocket Nothing Nothing
