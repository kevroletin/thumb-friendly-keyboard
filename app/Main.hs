module Main where

import System.IO
import qualified Data.Glome.Vec as V
import Data.List
import Control.Monad.State
import Data.Foldable

-- We assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"
data OldSocket = OldSocket Int deriving Show

data Switch = Switch {
  pos :: V.Vec,
  angles :: V.Vec
  } deriving Show;

type PolyhedronSurface = [Int]

data PolyhedronSocket = PolyhedronSocket {
  socketVertIds :: [Int]
  } deriving Show

type PolyhedronMonad a = State (Int, [V.Vec], [PolyhedronSurface]) a

data Envelope = Envelope {
  left  :: [V.Vec],
  right :: [V.Vec],
  front :: [V.Vec],
  back  :: [V.Vec]
  } deriving Show;

socketVert :: Int -> PolyhedronSocket -> Int
socketVert n (PolyhedronSocket xs) = xs !! n

addVertex :: V.Vec -> PolyhedronMonad Int
addVertex v = do (idx, vert', surf) <- get
                 put (idx + 1, vert' ++ [v], surf)
                 return idx

addSurface :: PolyhedronSurface -> PolyhedronMonad ()
addSurface surf = do (idx, vert', sx) <- get
                     put (idx, vert', sx ++ [surf])
                     return ()

addSurfaceSandwich :: PolyhedronSurface -> PolyhedronMonad ()
addSurfaceSandwich v = do addSurface v
                          addSurface (reverse $ map topToBottom v)

switchVertexes :: Switch -> [V.Vec]
switchVertexes (Switch centerPos (V.Vec ax ay az)) =
  [ move $ V.vec (-w) (-w) h
  , move $ V.vec (-w) w    h
  , move $ V.vec w    w    h
  , move $ V.vec w    (-w) h
  , move $ V.vec (-w) (-w) (-h)
  , move $ V.vec (-w) w    (-h)
  , move $ V.vec w    w    (-h)
  , move $ V.vec w    (-w) (-h)
  ]
  where
    rotation = V.compose [V.rotate (V.vec 1 0 0) (V.deg ax)
                         , V.rotate (V.vec 0 1 0) (V.deg ay)
                         , V.rotate (V.vec 0 0 1) (V.deg az)]
    move = V.xfm_point (V.xfm_mult (V.translate centerPos) rotation)
    w  = switchWidth / 2
    h  = switchHeight / 2

addSwitch :: Switch -> PolyhedronMonad PolyhedronSocket
addSwitch switch = fmap PolyhedronSocket $ mapM addVertex (switchVertexes switch)

addTopBottomSurf :: PolyhedronSocket -> PolyhedronMonad ()
addTopBottomSurf sw =
  addSurfaceSandwich $ map (socketVertIds sw !!) [0, 1, 2, 3]

addLeftToRightSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addLeftToRightSurf l r =
  addSurfaceSandwich [socketVert 1 r, socketVert 0 r, socketVert 3 l, socketVert 2 l]

addFrontToBackSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addFrontToBackSurf n f =
  addSurfaceSandwich [socketVert 1 n, socketVert 0 f, socketVert 3 f, socketVert 2 n]

addMiddleSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addMiddleSurf s0 s1 s2 s3 =
  addSurfaceSandwich [socketVert 2 s0, socketVert 3 s1, socketVert 0 s2, socketVert 1 s3]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = drop 2 $ scanl (\(_, b) c -> (b, c)) (head xs, head xs) xs

eachSquare :: [[a]]
           -> (a -> a -> a -> a -> PolyhedronMonad b)
           -> PolyhedronMonad ()
eachSquare sockets f = do
  for_ [0 .. (height - 2)] $ \y' ->
    for_ [0 .. (width - 2)] $ \x' -> do
        f (sockets !! y' !! x') (sockets !! (y' + 1) !! x') (sockets !! (y' + 1) !! (x' + 1)) (sockets !! y' !! (x' + 1))
  where
    width = length (head sockets)
    height = length sockets

addWall :: Int -> Int -> [PolyhedronSocket] -> (PolyhedronSurface -> PolyhedronSurface) -> PolyhedronMonad ()
addWall a b xs f = do
  let topLine    = concat $ map (\s -> [socketVert a s, socketVert b s]) xs
  let bottomLine = concat $ map (\s -> [socketVert (topToBottom b) s, socketVert (topToBottom a) s]) (reverse xs)
  let res = topLine ++ bottomLine
  addSurface(f res)

addFrontWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addFrontWall sockets = addWall 0 3 (head sockets) id

addBackWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addBackWall sockets = addWall 1 2 (last sockets) reverse

addLeftWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addLeftWall sockets = addWall 0 1 (map head sockets) reverse

addRightWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addRightWall sockets = addWall 3 2 (map last sockets) id

buildPlate :: [[Switch]] -> PolyhedronMonad ()
buildPlate switches = do
  sockets <- addSwitches switches
  for_ (concat sockets) addTopBottomSurf
  for_ sockets $ \line ->
    for_ (pairs line) (uncurry addLeftToRightSurf)
  for_ (transpose sockets) $ \row ->
    for_ (pairs row) (uncurry addFrontToBackSurf)
  eachSquare sockets addMiddleSurf
  addFrontWall sockets
  addBackWall sockets
  addLeftWall sockets
  addRightWall sockets
  return ()
  where
    addSwitches xs = sequence $ map (sequence . map addSwitch) xs

renderPolyhedron :: PolyhedronMonad [String]
renderPolyhedron = do (_, verts', surfs) <- get
                      return [
                        "polyhedron("
                        , "points=["
                        , intercalate ",\n" (fmap renderVec verts')
                        , "],"
                        , "faces=["
                        , intercalate ",\n" (fmap renderSurface surfs)
                        , "]);"
                        ]

evalPolyhedron :: PolyhedronMonad a -> [String]
evalPolyhedron m = evalState (m >> renderPolyhedron) (0, [], [])

switchWidth :: Double
switchWidth = 19.05

switchHeight :: Double
switchHeight = 5

holeWidth :: Double
holeWidth = 13.94

holeNotchWidth :: Double
holeNotchWidth = 0.5

holeNotchHeight :: Double
holeNotchHeight = 1.5

keycapBottomWidth :: Double
keycapBottomWidth = switchWidth

keycapTopWidth :: Double
keycapTopWidth = 14

keycapHeight :: Double
keycapHeight = 10

keycapElevation :: Double
keycapElevation = 6

envelopHeight :: Double
envelopHeight = 5

renderVec :: V.Vec -> String
renderVec (V.Vec x y z) = "[" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ "]"

renderSurface :: PolyhedronSurface -> String
renderSurface xs = show xs

-- It works only for polyhedron. Be careful not to pass bottom vertexes.
-- Probably it should be a part of PolyhedronMonad.
topToBottom :: Int -> Int
topToBottom = (+ 4)

renderHoleCube :: Switch -> [String]
renderHoleCube (Switch (V.Vec x y z) (V.Vec ax ay az)) = concat [
  cube holeWidth holeWidth (switchHeight + 2) (-1)
  , cube holeWidth (holeWidth + 2 * holeNotchWidth) switchHeight (-holeNotchHeight)
  ]
  where
    -- cube switchWidth switchWidth switchHeight will exactly match occupy
    -- switch space
    cube len_x len_y len_z dz = [
      "translate(" ++ show [x, y, z] ++ ")"
      , "rotate(" ++ show [ax, ay, az] ++ ")"
      , "translate(" ++ show [-(len_x / 2), -(len_y / 2), -(switchHeight / 2) + dz] ++ ")"
      , "cube(" ++ show [len_x, len_y, len_z] ++ ");"
      ]

renderKeycap :: Switch -> [String]
renderKeycap (Switch (V.Vec x y z) (V.Vec ax ay az)) = [
  "translate(" ++ show [x, y, z] ++ ")"
  , "rotate(" ++ show [ax, ay, az] ++ ")"
  , "hull() {"
  , "  translate(" ++ show [-hbw, -hbw, keycapElevation] ++ ")"
  , "    cube(" ++ show [keycapBottomWidth, keycapBottomWidth, 1] ++ ");"
  , "  translate(" ++ show [-htw, -htw, (keycapElevation + keycapHeight)] ++ ")"
  , "    cube(" ++ show [keycapTopWidth, keycapTopWidth, 1] ++ ");"
  , "};"
  ]
  where
    hbw = keycapBottomWidth / 2
    htw = keycapTopWidth / 2

renderHoles :: [Switch] -> [String]
renderHoles switches = concat (map renderHoleCube switches)

renderKeycaps :: [Switch] -> [String]
renderKeycaps switches = concat (map renderKeycap switches)

renderDifference :: [String] -> [String] -> [String]
renderDifference xs ys =
  "difference() {" : xs ++ ys ++ ["};"]

renderUnion :: [[String]] -> [String]
renderUnion xs =
  "union() {" : (concat xs) ++ ["};"]

color :: String -> [String] -> [String]
color c xs = "color(" : [show c] ++ [") {"] ++ xs ++ ["}"]

-- Connects plate with vertexes from envelope. Orientation of bottom paths is
-- the same as corresponding top paths.
-- plateTop -> plateBottom -> envelopeTop -> envelopeBottom
renderEnvelopePart :: [V.Vec] -> [V.Vec] -> [V.Vec] -> [V.Vec] -> [String]
renderEnvelopePart pt pb et eb = evalPolyhedron $
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

renderEnvelope :: [[Switch]] -> Maybe Envelope -> [String]
renderEnvelope _ Nothing = []
renderEnvelope switches (Just envelope) = concat [
  renderEnvelopePart frontWallTop frontWallBottom (front envelope) (lowerEnvelop $ front envelope)
  , renderEnvelopePart backWallTop backWallBottom (reverse $ back envelope) (reverse $ lowerEnvelop $ back envelope)
  , renderEnvelopePart rightWallTop rigthWallBottom (right envelope) (lowerEnvelop $ right envelope)
  , renderEnvelopePart leftWallTop leftWallBottom (reverse $ left envelope) (reverse $ lowerEnvelop $ left envelope)
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
    lowerEnvelop = map (\(V.Vec x y z) -> V.Vec x y (z - envelopHeight))

render :: [[Switch]] -> Maybe Envelope -> [String]
render switches envelope =
  renderUnion [[]
              , plate
              -- , (renderKeycaps $ concat switches)
              , renderEnvelope switches envelope
              ]
  where
    body = buildPlate switches >> renderPolyhedron
    platePolyhedron = evalState body (0, [], [])
    plate = renderDifference platePolyhedron (renderHoles $ concat switches)

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

main :: IO ()
main = do withFile "main_plate.scad"  WriteMode  $ \h -> mapM_ (hPutStrLn h) (render mainPlate (Just mainEnvelop))
          withFile "main_plate2.scad" WriteMode  $ \h -> mapM_ (hPutStrLn h) (render mainPlate Nothing)
          withFile "thumb_plate.scad" WriteMode $ \h -> mapM_ (hPutStrLn h) (render thumbPlate Nothing)
          withFile "single_socket.scad" WriteMode $ \h -> mapM_ (hPutStrLn h) (render singleSocket Nothing)
