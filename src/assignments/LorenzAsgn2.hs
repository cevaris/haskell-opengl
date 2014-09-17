import Numeric
import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw.ARB.WindowPos


----------------------------------------------------------------------------------------------------------------
-- Global State
type View = (GLfloat, GLfloat, GLfloat)

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   viewRot :: IORef View,
   angle'  :: IORef GLfloat,
   ph'     :: IORef GLfloat,
   th'     :: IORef GLfloat,
   info    :: IORef String
 }

makeState :: IO State
makeState = do
   f <- newIORef 0
   t <- newIORef 0
   v <- newIORef (0, 0, 0)
   a <- newIORef 0
   ph <- newIORef 0
   th <- newIORef 0
   i <- newIORef ""
   return $ State { frames = f, t0 = t, viewRot = v, angle' = a, ph' = ph, th' = th, info = i}

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

lorenzPoints :: State -> [(GLfloat,GLfloat,GLfloat)] 
lorenzPoints state = map (\(Lorenz i x y z) -> ((realToFrac x :: GLfloat), (realToFrac y :: GLfloat), (realToFrac z :: GLfloat))) (lorenz 0.001)

--rainbowColors = [(x,y,z) | x <- [1..5], y <- [1..5], z <- [1..5]]
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Grid

--zeroGrid = 0.0 :: GLfloat
--lineGrid = map (*1.0) [-10..10] :: [GLfloat]
--gridPoints = ([(x,zeroGrid,zeroGrid) | x <- lineGrid], [(zeroGrid,x,zeroGrid) | x <- lineGrid], [(zeroGrid,zeroGrid,x) | x <- lineGrid])

gridPoints :: [(GLfloat, GLfloat, GLfloat)]
gridPoints = [(0,0,0),(1,0,0),
              (0,0,0),(0,1,0),
              (0,0,0),(0,0,1)]
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Axes

--tickZero = 0.0 :: GLfloat
--tickLine = map (*1.0) [-1..1] :: [GLfloat]
--tickBase = map (*1.0) [-10..10] :: [GLfloat]
--tickPoints :: ([(GLfloat, GLfloat, GLfloat)], [(GLfloat, GLfloat, GLfloat)], [(GLfloat, GLfloat, GLfloat)])
--tickPoints = (chunksOf 3 $ [(a, b, tickZero) | a <- tickBase, b <- tickLine ], chunksOf 3 $ [(b, a, tickZero) | a <- tickBase, b <- tickLine ], chunksOf 3 $ [(tickZero, b, a) | a <- tickBase, b <- tickLine ])

----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Key Binding

--keyboard :: State -> KeyboardMouseCallback
--keyboard state (Char 'z')           _ _ _ = modRot state ( 0,  0,  5)
--keyboard state (Char 'Z')           _ _ _ = modRot state ( 0,  0, -5)
--keyboard state (SpecialKey KeyUp)   _ _ _ = modRot state (-5,  0,  0)
--keyboard state (SpecialKey KeyDown) _ _ _ = modRot state ( 5,  0,  0)
--keyboard state (SpecialKey KeyLeft) _ _ _ = modRot state ( 0, -5,  0)
--keyboard state (SpecialKey KeyRight)_ _ _ = modRot state ( 0,  5,  0)
--keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
--keyboard _     _                    _ _ _ = return ()

keyboard :: State -> KeyboardMouseCallback
--keyboard state (Char 'z')           _ _ _ = modRot state ( 0,  0,  5)
--keyboard state (Char 'Z')           _ _ _ = modRot state ( 0,  0, -5)
keyboard state (SpecialKey KeyUp)   _ _ _ = modRot state KeyUp
keyboard state (SpecialKey KeyDown) _ _ _ = modRot state KeyDown
keyboard state (SpecialKey KeyLeft) _ _ _ = modRot state KeyLeft
keyboard state (SpecialKey KeyRight)_ _ _ = modRot state KeyRight
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


----------------------------------------------------------------------------------------------------------------
--toInteger $ round  (33 :: GLfloat)
modRot :: State -> SpecialKey -> IO ()
modRot state KeyDown = do
  ph' state $~! (+5)
  (viewRot state) $~! (\(x, y, z) -> (x-5, y, z))
  --(x, y, z) <- get (viewRot state)
  --viewRot state $= (x + 5, y + 0, z + 0)
  postRedisplay Nothing
modRot state KeyUp  = do
  ph' state $~! (\x -> x - 5)
  (viewRot state) $~! (\(x, y, z) -> (x+5, y, z))
  postRedisplay Nothing
  --(x, y, z) <- get (viewRot state)
  --viewRot state $= (x + dx, y + dy, z + dz)
modRot state KeyRight = do
  th' state $~! (+5)
  (viewRot state) $~! (\(x, y, z) -> (x, y-5, z))
  postRedisplay Nothing
  --(x, y, z) <- get (viewRot state)
  --viewRot state $= (x + dx, y + dy, z + dz)
modRot state KeyLeft = do
  th' state $~!(\x -> x - 5)
  (viewRot state) $~! (\(x, y, z) -> (x, y+5, z))
  postRedisplay Nothing
  --(x, y, z) <- get (viewRot state)
  --viewRot state $= (x + dx, y + dy, z + dz)


idle :: State -> IdleCallback
idle state = do
   --angle' state $~! (+2)
   postRedisplay Nothing

visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing

reshape :: ReshapeCallback
reshape s@(Size width height) = do
   let h = fromIntegral height / fromIntegral width

   viewport $= (Position 0 0, s)
   matrixMode $= Projection
   loadIdentity
   --frustum (-1) 1 (-h) h 0 0
   --frustum (-1) 1 (-h) h 5 60
   frustum (-h) h (-h) h 30 60
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-40 :: GLfloat))


--setupProjection :: ReshapeCallback
--setupProjection (Size width height) = do
--  -- don't want a divide by zero
--  --let h = max 1 height
--  -- reset the viewport to new dimensions
--  viewport $= (Position 0 0, Size width height)
--  -- set projection matrix as the current matrix
--  matrixMode $= Projection
--  -- reset projection matrix
--  loadIdentity
--  -- calculate aspect ratio of window
--  --perspective 52 (fromIntegral width / fromIntegral h) 1 1000
--  --perspective 52 (fromIntegral width / fromIntegral h) 1 1
--  -- set modelview matrix
--  matrixMode $= Modelview 0
--  -- reset modelview matrix
--  loadIdentity


-- Set Vertex2
vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y = vertex $ Vertex2 x y
  

-- Set Vertex3
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

-- Set Vertex4
vertex4f :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 x y z w

glWindowPos :: GLfloat -> GLfloat -> IO ()
glWindowPos x y = glWindowPos2f x y


updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    angle <- get (angle' state)
    view <- get (viewRot state)
    ph <- get (ph' state)
    th <- get (th' state)
    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        --result = (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS" ++ " ph " ++ show ph ++ " th " ++ show th)
        result = ("[" ++ show f ++ " frames in " ++  (showGFloat (Just 2) seconds "") ++ " seconds] ["++ (showGFloat (Just 2) fps "") ++ " FPS]" ++ " [ph " ++ show ph ++ "] [th " ++ show th ++ "] ["  ++ show view ++ "]")
    info state $= result
    --putStrLn 
    t0 state $= t
    frames state $= 0


draw :: State -> (DisplayList, DisplayList) -> IO ()
draw state (lorenzAttractor, grid) = do
    
  clear [ ColorBuffer, DepthBuffer ]

  (x, y, z) <- get (viewRot state)

  info <- get (info state)

  preservingMatrix $ do
    rotate x (Vector3 1 0 0)
    rotate y (Vector3 0 1 0)
    rotate z (Vector3 0 0 1)

    preservingMatrix $ do   
      lineWidth $= 0.5
      scale 0.019 0.019 (0.019::GLfloat)
      callList lorenzAttractor
    
    preservingMatrix $ do
      lineWidth $= 2
      scale 0.5 0.5 (0.5::GLfloat)
      callList grid

    preservingMatrix $ do
      currentRasterPosition $= vertex4f 0.5 0 0 1
      renderString Helvetica18 $ "X"
      currentRasterPosition $= vertex4f 0 0.5 0 1
      renderString Helvetica18 $ "Y"
      currentRasterPosition $= vertex4f 0 0 0.5 1
      renderString Helvetica18 $ "Z"
      currentRasterPosition $= vertex4f 0 0 0 1

    preservingMatrix $ do
      glWindowPos 5 5
      renderString Helvetica18 $ info

  swapBuffers
  updateInfo state
  
  -- Rotate
  --ph <- get (ph' state)
  --th <- get (th' state)
  --info <- get (info state)
  
  --loadIdentity

  --rotate ph (Vector3 1 0 0)
  --rotate th (Vector3 0 1 0)
  
  --preservingMatrix $ do   
  --  lineWidth $= 0.5
  --  scale 0.019 0.019 (0.019::GLfloat)
  --  callList lorenzAttractor
  
  --preservingMatrix $ do
  --  lineWidth $= 2
  --  scale 0.5 0.5 (0.5::GLfloat)
  --  callList grid

  --preservingMatrix $ do
  --  currentRasterPosition $= vertex4f 0.5 0 0 1
  --  renderString Helvetica18 $ "X"
  --  currentRasterPosition $= vertex4f 0 0.5 0 1
  --  renderString Helvetica18 $ "Y"
  --  currentRasterPosition $= vertex4f 0 0 0.5 1
  --  renderString Helvetica18 $ "Z"
  --  currentRasterPosition $= vertex4f 0 0 0 1

  --preservingMatrix $ do
  --  glWindowPos 5 5
  --  renderString Helvetica18 $ info


  --swapBuffers
  --updateInfo state
  


myInit :: [String] -> State -> IO (DisplayList, DisplayList)
myInit args state = do
  --position (Light 0) $= Vertex4 5 5 15 0
  --cullFace $= Just Back
  --lighting $= Enabled
  --light (Light 0) $= Enabled
  --depthFunc $= Just Less

  lorenzAttractor <- defineNewList Compile $ do
    renderPrimitive LineStrip $ do
      mapM_ (\(x, y, z) -> vertex3f x y z ) (lorenzPoints state)


  grid <- defineNewList Compile $ do
    renderPrimitive Lines $ do
      mapM_ (\(x, y, z) -> vertex3f x y z ) gridPoints

  return (lorenzAttractor, grid)

----------------------------------------------------------------------------------------------------------------
-- Key Binding

main :: IO ()
main = do
    initialWindowSize $= Size 700 700
    (_progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    
    initialWindowPosition $= Position 500 500
    _window <- createWindow "Lorenz Attractor - Adam Cardenas"

    state <- makeState
    (lorenzObject, gridObj) <- myInit args state

    displayCallback $= draw state (lorenzObject, gridObj)
    reshapeCallback $= Just reshape
    --reshapeCallback $= Just setupProjection

    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


