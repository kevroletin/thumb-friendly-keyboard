{-| Envelope is outer part of a plate. It contains no switches and it's only
   purpose is a smooth transition from curly shape to something regular like a
   parallelepiped.
-}
module Keyboard.Parts.Envelope (
  Envelope(..)
  , buildEnvelope
  , envelopeFrontWall
  , envelopeBackWall
  , envelopeRightWall
  , envelopeLeftWall
) where

import qualified Data.Glome.Vec          as V
import           Keyboard.Config
import           Keyboard.Parts.Plate
import           Keyboard.Parts.Switch
import           Keyboard.Scad
import           Keyboard.Scad.Primitives
import           Keyboard.Scad.Sandwidge
import           Keyboard.Transformation
import           Keyboard.Scad.HollowFigure

data Envelope = Envelope {
  left        :: [V.Vec],
  right       :: [V.Vec],
  front       :: [V.Vec],
  back        :: [V.Vec],
  upDirection :: V.Vec
  } deriving Show;

instance Transformable Envelope where
  transform = transformEnvelope

transformEnvelope :: Transformation -> Envelope -> Envelope
transformEnvelope tr0 (Envelope l r f b u) =
  Envelope (t l) (t r) (t f) (t b) (rotateOnly u)
  where
    t = map (transform tr0)
    rotateOnly :: V.Vec -> V.Vec
    rotateOnly = case tr0 of
      tr@(Rotate _) -> transform tr
      _             ->id

buildEnvelopePart
  :: Wall V.Vec -> Wall V.Vec -> ScadProgram
buildEnvelopePart (Wall pt pb) (Wall et eb)
  | any null [pt, pb, et, eb] = dummyFigure
  | otherwise = buildPolyhedron $
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

{-|Shifts each point "down" using envelope's "up" vector
-}
translateEnvelopLine :: Envelope -> [V.Vec] -> [V.Vec]
translateEnvelopLine envelope =
  map (\(V.Vec x y z) -> V.vscaleadd (V.Vec x y z) (upDirection envelope) (-envelopHeight))

envelopeFrontWall :: Envelope -> Wall V.Vec
envelopeFrontWall envelope = Wall
  (front envelope) (translateEnvelopLine envelope $ front envelope)

envelopeBackWall :: Envelope -> Wall V.Vec
envelopeBackWall e = Wall
  (reverse $ back e) (reverse $ translateEnvelopLine e $ back e)

envelopeRightWall :: Envelope -> Wall V.Vec
envelopeRightWall e = Wall (right e) (translateEnvelopLine e $ right e)

envelopeLeftWall :: Envelope -> Wall V.Vec
envelopeLeftWall e = Wall (reverse $ left e) (reverse $ translateEnvelopLine e $ left e)

buildEnvelope :: Plate -> Envelope -> HollowFigure
buildEnvelope p e = HollowFigure (Just res) Nothing
  where res = union [
          buildEnvelopePart (plateFrontWall p) (envelopeFrontWall e)
          , buildEnvelopePart (plateBackWall p) (envelopeBackWall e)
          , buildEnvelopePart (plateRightWall p) (envelopeRightWall e)
          , buildEnvelopePart (plateLeftWall p) (envelopeLeftWall e)
          ]
