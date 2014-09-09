import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )

----------------------------------------------------------------------------------------------------------------
-- Global State
type View = (GLfloat, GLfloat, GLfloat)

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   viewRot :: IORef View,
   angle'  :: IORef GLfloat }

makeState :: IO State
makeState = do
   f <- newIORef 0
   t <- newIORef 0
   v <- newIORef (20, 30, 0)
   a <- newIORef 0
   return $ State { frames = f, t0 = t, viewRot = v, angle' = a }

----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Lorenz

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

----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Key Binding

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char 'z')           _ _ _ = modRot state ( 0,  0,  5)
keyboard state (Char 'Z')           _ _ _ = modRot state ( 0,  0, -5)
keyboard state (SpecialKey KeyUp)   _ _ _ = modRot state ( 5,  0,  0)
keyboard state (SpecialKey KeyDown) _ _ _ = modRot state (-5,  0,  0)
keyboard state (SpecialKey KeyLeft) _ _ _ = modRot state ( 0,  5,  0)
keyboard state (SpecialKey KeyRight)_ _ _ = modRot state ( 0, -5,  0)
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()

modRot :: State -> View -> IO ()
modRot state (dx,dy,dz) = do
   (x, y, z) <- get (viewRot state)
   viewRot state $= (x + dx, y + dy, z + dz)
   postRedisplay Nothing


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
  state <- makeState

  -- http://stackoverflow.com/questions/9300773/drawing-lines-with-opengl-in-haskell#
  renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z) -> vertex3f x y z ) lorenzPoints
  flush



