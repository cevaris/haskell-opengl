import Graphics.UI.GLUT

data CustomColor = CustomColor {r::GLfloat, g::GLfloat, b::GLfloat} deriving (Read, Show, Eq)

--r :: CustomColor -> GLfloat

--type RColor = GLfloat
--type GColor = GLfloat
--type BColor = GLfloat

--data CustomColor = CustomColor RColor GColor BColor deriving (Read, Show, Eq)

red   = CustomColor 1 0 0
green = CustomColor 0 1 0
blue  = CustomColor 0 0 1

--red   = 1 0 0 :: CustomColor
--green = 0 1 0 :: CustomColor
--blue  = 0 0 1 :: CustomColor

trianglePoints :: [(GLfloat,GLfloat,GLfloat,CustomColor)]
trianglePoints = [(0, 0.5, 0, blue), (0.5, (-0.5), 0, red), ((-0.5), (-0.5), 0, red)]
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop


color3f :: CustomColor -> IO ()
color3f c = color $ Color3 (r c) (g c) (b c)

color3f :: CustomColor -> IO ()
color3f c = color $ Color3 (r c) (g c) (b c)

--vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

--colorVertex :: GLfloat -> GLfloat -> GLfloat -> CustomColor -> Vertex3
--colorVertex x y z c = vertex $ Vertex3 1 2 3
--colorVertex :: x y z c = do 
--    Color3f (r c) (b c) (g c)
--    Vertex3 x y z


display :: DisplayCallback
display = do 
  clear [ColorBuffer]

  -- http://stackoverflow.com/questions/9300773/drawing-lines-with-opengl-in-haskell#
  renderPrimitive Polygon $ do
    mapM_ (\(x, y, z, c) -> vertex $ Vertex3 x y z ) trianglePoints
  flush



