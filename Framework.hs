module Framework where
import Graphics.UI.GLUT


framework defaults init display idle reshape keyboard = do
  (progname, args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer, RGBMode]
  initialWindowSize $= Size 400 300
  initialWindowPosition $= Position 300 200
  
  defaults
  
  createWindow progname
  
  init args
  
  displayCallback $= display
  idleCallback $= Just idle
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboard
  
  mainLoop