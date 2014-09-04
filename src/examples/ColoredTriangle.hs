import Graphics.UI.GLUT

data CustomColor = CustomColor {r::GLfloat, g::GLfloat, b::GLfloat} deriving (Read, Show, Eq)

red   = CustomColor 1 0 0
green = CustomColor 0 1 0
blue  = CustomColor 0 0 1

trianglePoints :: [(GLfloat,GLfloat,GLfloat,CustomColor)]
trianglePoints = [(0, 0.5, 0, blue), (0.5, (-0.5), 0, green), ((-0.5), (-0.5), 0, red)]
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
  
-- Set color
color3f :: CustomColor -> IO ()
color3f c = color $ Color3 (r c) (g c) (b c)

-- Set Vertex
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

display :: DisplayCallback
display = do 
  clear [ColorBuffer]

  -- http://stackoverflow.com/questions/9300773/drawing-lines-with-opengl-in-haskell#
  renderPrimitive Polygon $ do
    mapM_ (
      \(x, y, z, c) -> do 
        color3f  c
        vertex3f x y z ) trianglePoints
  flush



