{-| Envelope is outer part of a plate. It contains no switches and it's only
   purpose is a smooth transition from curly shape to something regular like a
   parallelepiped.
-}
module Parts.Envelope (
  buildEnvelope
) where

import qualified Data.Glome.Vec as V
import Keyboard
import Config
import Scad
import Scad.Builders
import Parts.Common

-- Connects plate with vertexes from envelope. Orientation of bottom paths is
-- the same as corresponding top paths.
-- plateTop -> plateBottom -> envelopeTop -> envelopeBottom
buildEnvelopePart :: [V.Vec] -> [V.Vec] -> [V.Vec] -> [V.Vec]
                   -> ScadProgram
buildEnvelopePart pt pb et eb = buildPolyhedron $
  do pt'<- mapM addVertex pt
     et'<- mapM addVertex et
     pb'<- mapM addVertex pb
     eb'<- mapM addVertex eb
     addSurface (pt' ++ reverse et')
     addSurface (reverse pb' ++ eb')
     addSurface (reverse pt' ++ pb')
     -- Open Scad sometimes uses ugly tessellation, so draw "by hands"
     -- addSurface (et ++ reverse eb')
     eachSquare [et', eb'] $ \t0 t1 t2 t4 -> addSurface [t4, t2, t1, t0]
     addSurface [last pb', last eb', last et', last pt']
     addSurface [head pt', head et', head eb', head pb']

buildEnvelope :: [[Switch]] -> Maybe Envelope -> ScadProgram
buildEnvelope _ Nothing = dummyFigure
buildEnvelope switches (Just envelope) = union [
  buildEnvelopePart frontWallTop frontWallBottom (front envelope) (lowerEnvelop $ front envelope)
  , buildEnvelopePart backWallTop backWallBottom (reverse $ back envelope) (reverse $ lowerEnvelop $ back envelope)
  , buildEnvelopePart rightWallTop rigthWallBottom (right envelope) (lowerEnvelop $ right envelope)
  , buildEnvelopePart leftWallTop leftWallBottom (reverse $ left envelope) (reverse $ lowerEnvelop $ left envelope)
  ]
  where
    frontWallTop    = (concat $ map (\x -> [x !! 0, x !! 3]) $ map switchVertexes (head switches))
    frontWallBottom = (concat $ map (\x -> [x !! 4, x !! 7]) $ map switchVertexes (head switches))
    backWallTop     = (concat $ map (\x -> [x !! 2, x !! 1]) $ reverse $ map switchVertexes (last switches))
    backWallBottom  = (concat $ map (\x -> [x !! 6, x !! 5]) $ reverse $ map switchVertexes (last switches))
    rightWallTop    = (concat $ map (\x -> [x !! 3, x !! 2]) $ map switchVertexes (map last switches))
    rigthWallBottom = (concat $ map (\x -> [x !! 7, x !! 6]) $ map switchVertexes (map last switches))
    leftWallTop     = (concat $ map (\x -> [x !! 1, x !! 0]) $ reverse $ map switchVertexes (map head switches))
    leftWallBottom  = (concat $ map (\x -> [x !! 5, x !! 4]) $ reverse $ map switchVertexes (map head switches))
    lowerEnvelop = map (\(V.Vec x y z) -> V.vscaleadd (V.Vec x y z) (upDirection envelope) (-envelopHeight))
