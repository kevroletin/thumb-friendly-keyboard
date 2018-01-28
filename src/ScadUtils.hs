module ScadUtils (
  ScadProgram
  , render
  , renderToString
  , hull
  , union
  , difference
  , cube
  , translate
  , rotate
  , color
  , dummyFigure
  , block

  , PolyhedronSurface
  , PolyhedronMonad
  , addVertex
  , addSurface
  , getCurrentPolyhedron
  , buildPolyhedron

) where

import           Control.Monad.State
import qualified Data.Glome.Vec      as V
import qualified Data.List           as List

type PolyhedronSurface = [Int]

type PolyhedronMonad a = State (Int, [V.Vec], [PolyhedronSurface]) a

data OperatorParams = NoParams
                    | VecParams V.Vec
                    | StringParams String
                    deriving Show

-- + we can create transformation of transformation which doesn't make sense;
-- + Monoid to compose figures using <> instead of union;
-- + implement comments to make resulting scad program debuggable.
data ScadProgram =
  Polyhedron { polyhedronPoints :: [V.Vec]
             , polyhedronFaces  :: [PolyhedronSurface]}
  | Cube V.Vec
  | Block [ScadProgram]
  | Operator String OperatorParams ScadProgram
  deriving Show

isBlcok :: ScadProgram -> Bool
isBlcok (Block _) = True
isBlcok _         = False

addVertex :: V.Vec -> PolyhedronMonad Int
addVertex v = do (idx, vert', surf) <- get
                 put (idx + 1, vert' ++ [v], surf)
                 return idx

addSurface :: PolyhedronSurface -> PolyhedronMonad ()
addSurface surf = do (idx, vert', sx) <- get
                     put (idx, vert', sx ++ [surf])
                     return ()

getCurrentPolyhedron :: PolyhedronMonad ScadProgram
getCurrentPolyhedron = do (_, verts, surfs) <- get
                          return $ Polyhedron verts surfs

buildPolyhedron :: PolyhedronMonad a -> ScadProgram
buildPolyhedron m = evalState (m >> getCurrentPolyhedron) (0, [], [])

renderVec :: V.Vec -> String
renderVec (V.Vec x y z) = show [x, y, z]

renderSurface :: PolyhedronSurface -> String
renderSurface xs = show xs

render :: ScadProgram -> [String]
render (Polyhedron verts faces) =
  [
    "polyhedron("
  , "points=["
  , List.intercalate ",\n" (map renderVec verts)
  , "],"
  , "faces=["
  , List.intercalate ",\n" (map renderSurface faces)
  , "]);"
  ]
render (Cube pos) = ["cube(" ++ renderVec pos ++ ");"]
render (Operator name params child) =
  if isBlcok child
    then (title ++ " {") : render child ++ ["};"]
    else title : render child
  where paramsStr NoParams      = ""
        paramsStr (VecParams v) = renderVec v
        title = (name ++ "(" ++ (paramsStr params) ++ ")")
render (Block childs) = concatMap render childs

renderToString :: ScadProgram -> [Char]
renderToString = (List.intercalate "\n") . render

hull :: [ScadProgram] -> ScadProgram
hull xs = Operator "hull" NoParams (Block xs)

union :: [ScadProgram] -> ScadProgram
union xs = Operator "union" NoParams (Block xs)

difference :: [ScadProgram] -> ScadProgram
difference xs = Operator "difference" NoParams (Block xs)

cube :: V.Flt -> V.Flt -> V.Flt -> ScadProgram
cube x y z = Cube (V.Vec x y z)

translate :: V.Flt -> V.Flt -> V.Flt -> ScadProgram -> ScadProgram
translate x y z = Operator "translate" (VecParams $ V.Vec x y z)

rotate :: V.Flt -> V.Flt -> V.Flt -> ScadProgram -> ScadProgram
rotate x y z =  Operator "rotate" (VecParams $ V.Vec x y z)

color :: String -> ScadProgram -> ScadProgram
color c = Operator "color" (StringParams c)

dummyFigure :: ScadProgram
dummyFigure = Block []

block :: [ScadProgram] -> ScadProgram
block = Block
