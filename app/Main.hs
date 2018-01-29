module Main where

import System.IO
import qualified Data.Glome.Vec as V
import qualified Data.List as List
import Control.Monad.State
import Data.Foldable

import Scad
import Scad.Sandwidge
import GeneralUtils
import Keyboard
import Config
import Parts

-- We assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"

buildFinalPart :: [[Switch]] -> Maybe Envelope -> String
buildFinalPart switches envelope =
  renderToString $ union [
          buildPlate switches
        , (buildKeycaps $ concat switches)
        , buildEnvelope switches envelope
        ]

mainPlate :: [[Switch]]
mainPlate = [
 [ Switch (V.vec 0   0    7)  (V.vec (-25) 10    5)
 , Switch (V.vec 22  2    4)  (V.vec (-25) 0     5)
 , Switch (V.vec 44  2    4)  (V.vec (-25) 0     0)
 , Switch (V.vec 66  4    4)  (V.vec (-25) 0     0)
 , Switch (V.vec 90  (-2) 6)  (V.vec (-25) (-10) (-15))
 , Switch (V.vec 111 (-6) 12) (V.vec (-25) (-20) (-15))
 ],
 [ Switch (V.vec 0   23   2)  (V.vec (0)   10    5)
 , Switch (V.vec 22  25   0)  (V.vec (0)   0     5)
 , Switch (V.vec 44  28   0)  (V.vec (0)   0     0)
 , Switch (V.vec 66  30   0)  (V.vec (0)   0     0)
 , Switch (V.vec 90  22   2)  (V.vec (0)   (-10) (-15))
 , Switch (V.vec 111 18   09) (V.vec (0)   (-20) (-15))
 ],
 [ Switch (V.vec 0   46   8)  (V.vec (25)  10    5)
 , Switch (V.vec 22  48   6)  (V.vec (25)  0     5)
 , Switch (V.vec 44  54   6)  (V.vec (25)  0     0)
 , Switch (V.vec 66  56   6)  (V.vec (25)  0     0)
 , Switch (V.vec 90  46   8)  (V.vec (25)  (-10) (-15))
 , Switch (V.vec 111 42   15) (V.vec (30)  (-23) (-15))
 ]
 ]

 -- From left to right from front to back
 -- Front and back sides are in same order. The same with left and right.
mainEnvelop :: Envelope
mainEnvelop = Envelope {
    front = [V.vec (-20) (-20) 10, V.vec 50 (-20) 10]
  , back  = [V.vec (-20) (70) 10, V.vec 140 (70) 10]
  , left  = [head (front mainEnvelop), head (back mainEnvelop)]
  , right = [V.vec 140 (10) 10, last (back mainEnvelop)]
  }

thumbPlate :: [[Switch]]
thumbPlate = [
 [ Switch (V.vec 0  0  6) (V.vec (-15) (15)  0)
 , Switch (V.vec 22 0  3) (V.vec (-15) 0     0)
 , Switch (V.vec 44 0  6) (V.vec (-15) (-15) 0)
 ],
 [ Switch (V.vec 0  22 3) (V.vec 0     (15)  0)
 , Switch (V.vec 22 22 0) (V.vec 0     0     0)
 , Switch (V.vec 44 22 3) (V.vec 0     (-15) 0)
 ],
 [ Switch (V.vec 0  44 6) (V.vec (15)  (15)  0)
 , Switch (V.vec 22 44 3) (V.vec (15)  0     0)
 , Switch (V.vec 44 44 6) (V.vec (15)  (-15) 0)
 ]
 ]

singleSocket :: [[Switch]]
singleSocket = [[Switch (V.vec 0 0 0) (V.vec 0 0 0)]]

-- main :: IO ()
-- main = do withFile "main_plate.scad"  WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate (Just mainEnvelop)
--           withFile "main_plate2.scad" WriteMode  $ \h -> hPutStrLn h $ buildFinalPart mainPlate Nothing
--           withFile "thumb_plate.scad" WriteMode $ \h -> hPutStrLn h $  buildFinalPart thumbPlate Nothing
--           withFile "single_socket.scad" WriteMode $ \h -> hPutStrLn h $ buildFinalPart singleSocket Nothing


test = let topSquare = [V.vec (-10) (-10) 50, V.vec (-10) 10 50,
                       V.vec 10 10 40,       V.vec 10 (-10) 40]
           lower (V.Vec x y z) = V.vec x y (z - 3)
           lowerSquare = map lower topSquare
           in Sandwidge topSquare lowerSquare

a = translate 10 10 10 $ block [buildSadwidge test
                               , projectionDown test
                               ]

b = union [ buildSadwidge test
          , projectionDown test
          ]

main = do withFile "test.scad" WriteMode $ \h ->
            hPutStrLn h (renderToString $ combineConvexShells a b)
            -- hPutStrLn h (renderToString test2)
