import Graphics.UI.GLUT
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  window <- createWindow "Hello World Sammy!!"
  displayCallback $= display
  -- GLUT takes over
  mainLoop 
 
display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush