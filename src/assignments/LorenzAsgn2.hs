import Graphics.UI.GLUT
import Control.Monad ( when )
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

----------------------------------------------------------------------------------------------------------------


modRot :: State -> View -> IO ()
modRot state (dx,dy,dz) = do
   (x, y, z) <- get (viewRot state)
   viewRot state $= (x + dx, y + dy, z + dz)
   postRedisplay Nothing

idle :: State -> IdleCallback
idle state = do
   angle' state $~! (+2)
   postRedisplay Nothing

visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing

--reshape :: ReshapeCallback
--reshape size = do
--  viewport $= (Position 0 0, size)
--  postRedisplay Nothing
-- new window size or exposure
reshape :: ReshapeCallback
reshape s@(Size width height) = do
   let h = fromIntegral height / fromIntegral width

   viewport $= (Position 0 0, s)
   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-h) h 5 60
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-40 :: GLfloat))
  
---- Set color
--color3f :: CustomColor -> IO ()
--color3f c = color $ Color3 (r c) (g c) (b c)

-- Set Vertex
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z


draw :: DisplayList -> State -> IO ()
draw obj1 state = do
  clear [ ColorBuffer, DepthBuffer ]
  (x, y, z) <- get (viewRot state)
  a <- get (angle' state)

  let translatef = translate :: Vector3 GLfloat -> IO ()
  preservingMatrix $ do
    rotate x (Vector3 1 0 0)
    rotate y (Vector3 0 1 0)
    rotate z (Vector3 0 0 1)

  preservingMatrix $ do
    translatef (Vector3 (-3) (-2) 0)
    rotate a (Vector3 0 0 1)
    callList obj1

  swapBuffers
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
    putStrLn (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS")
    t0 state $= t
    frames state $= 0


myInit :: [String] -> IO DisplayList
myInit args = do
  l <- defineNewList Compile $ do
    renderPrimitive LineStrip $ do
      mapM_ (\(x, y, z) -> vertex3f x y z ) lorenzPoints
  return l
   --position (Light 0) $= Vertex4 5 5 10 0
   --cullFace $= Just Back
   --lighting $= Enabled
   --light (Light 0) $= Enabled
   --depthFunc $= Just Less

   -- make the gears
   --g1 <- defineNewList Compile $ do
   --   --materialAmbientAndDiffuse Front $= Color4 0.8 0.1 0.0 1.0
   --   --gear 1 4 1 20 0.7

   --g2 <- defineNewList Compile $ do
   --   materialAmbientAndDiffuse Front $= Color4 0.0 0.8 0.2 1.0
   --   gear 0.5 2 2 10 0.7

   --g3 <- defineNewList Compile $ do
   --   materialAmbientAndDiffuse Front $= Color4 0.2 0.2 1.0 1.0
   --   gear 1.3 2 0.5 10 0.7

   --normalize $= Enabled

   --return g1

--display :: DisplayCallback
--display = do 
--  clear [ColorBuffer]
--  state <- makeState

--  -- http://stackoverflow.com/questions/9300773/drawing-lines-with-opengl-in-haskell#
--  renderPrimitive LineStrip $ do
--    mapM_ (\(x, y, z) -> vertex3f x y z ) lorenzPoints
--  flush


--draw :: (DisplayList,DisplayList,DisplayList,Int) -> State -> IO ()
--draw (gear1, gear2, gear3, autoexit) state = do
--   clear [ ColorBuffer, DepthBuffer ]
--   (x, y, z) <- get (viewRot state)
--   a <- get (angle' state)

--   let translatef = translate :: Vector3 GLfloat -> IO ()
--   preservingMatrix $ do
--      rotate x (Vector3 1 0 0)
--      rotate y (Vector3 0 1 0)
--      rotate z (Vector3 0 0 1)

--      preservingMatrix $ do
--         translatef (Vector3 (-3) (-2) 0)
--         rotate a (Vector3 0 0 1)
--         callList gear1

--      preservingMatrix $ do
--         translatef (Vector3 3.1 (-2) 0)
--         rotate (-2 * a - 9) (Vector3 0 0 1)
--         callList gear2

--      preservingMatrix $ do
--         translatef (Vector3 (-3.1) 4.2 0)
--         rotate (-2 * a - 25) (Vector3 0 0 1)
--         callList gear3

----------------------------------------------------------------------------------------------------------------
-- Key Binding

main :: IO ()
main = do
    (_progName, args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]

    initialWindowPosition $= Position 0 0
    initialWindowSize $= Size 300 300
    state <- makeState
    lorenzObject <- myInit args

    displayCallback $= draw lorenzObject state
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  --(_progName, _args) <- getArgsAndInitialize
  --_window <- createWindow "Hello World"
  --displayCallback $= display
  --reshapeCallback $= Just reshape
  --mainLoop