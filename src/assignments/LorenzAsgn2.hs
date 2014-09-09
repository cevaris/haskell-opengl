import Graphics.UI.GLUT

s  = 10
b  = 2.6666
r  = 28
dt = 0.001

data Lorenz = Lorenz {step::Integer, x::Float, y::Float, z::Float} deriving (Read, Show, Eq)

lzBase   = Lorenz 0 1 1 1

lorenz  :: Float -> [Lorenz]
lorenz  dt = go lzBase [lzBase]
        where 
          go :: Lorenz -> [Lorenz] -> [Lorenz]
          go (Lorenz 50000 _ _ _) xs = reverse xs
          go (Lorenz i x y z)  xs = let l = Lorenz (i+1) (x+dt*(s*(y-x))) (y+dt*(x*(r-z)-y)) (z+dt*(x*y-b*z))
                                    in go l (l:xs)

lorenzPoints :: [(GLfloat,GLfloat,GLfloat)]
lorenzPoints = map (\(Lorenz i x y z) -> ((realToFrac (x/10) :: GLfloat), (realToFrac (y/10) :: GLfloat), (realToFrac (z/10) :: GLfloat))) (lorenz 0.001)

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
  
---- Set color
--color3f :: CustomColor -> IO ()
--color3f c = color $ Color3 (r c) (g c) (b c)

-- Set Vertex
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

display :: DisplayCallback
display = do 
  clear [ColorBuffer]

  -- http://stackoverflow.com/questions/9300773/drawing-lines-with-opengl-in-haskell#
  renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z) -> vertex3f x y z ) lorenzPoints
  flush



