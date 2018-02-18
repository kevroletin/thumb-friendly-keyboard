module Main where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.List              as List
import           System.IO

import           Scad
-- TODO: main shouldn't contain rendering logic, only configuration
import           Config
import           GeneralUtils
import           Parts
import           Scad
import qualified Scad.Connectors.Sphere as Sp
import           Transformation

buildFinalPart :: [[Switch]] -> Maybe Envelope -> Maybe (Double, Double)
               -> String
buildFinalPart switches envelope rectEnvelope =
  renderToScad $ difference [
    union [
        buildPlate switches
        , (buildKeycaps $ concat switches)
        , buildEnvelope switches envelope
        , maybeRectEnvelope rectEnvelope
        ]
    , buildHoles switches
    , maybeRectEnvelopeHole rectEnvelope
    ]
  where
    env x_len y_len = (computeRectEnvelopeAroundPlate switches x_len y_len 2.5)
    maybeRectEnvelopeHole Nothing = dummyFigure
    maybeRectEnvelopeHole (Just (x_len, y_len)) =
      buildRectEnvelopeHole (env x_len y_len)
    maybeRectEnvelope Nothing = dummyFigure
    maybeRectEnvelope (Just (x_len, y_len)) =
      buildRectEnvelope switches (env x_len y_len)

mainPlate :: [[Switch]]
mainPlate = fmap (fmap $ transform (Translate $ vec 0 0 (-5))) [
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

main :: IO ()
main = do withFile "main_plate.scad"  WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing (Just mainRectEnvelope)
          withFile "main_plate2.scad" WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing Nothing
          withFile "thumb_plate.scad" WriteMode $ \h -> hPutStrLn h $  buildFinalPart thumbPlate Nothing (Just thumbRectEnvelope)
          withFile "single_socket.scad" WriteMode $ \h -> hPutStrLn h $ buildFinalPart singleSocket Nothing Nothing
