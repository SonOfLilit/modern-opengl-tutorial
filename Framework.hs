module Framework where
import Graphics.UI.GLUT


framework defaults init display idle reshape keyboard = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode]
  initialWindowSize $= Size 400 300
  initialWindowPosition $= Position 300 200
  
  defaults
  
  createWindow progname
  
  init
  
  displayCallback $= display
  idleCallback $= Just idle
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboard
  
  mainLoop