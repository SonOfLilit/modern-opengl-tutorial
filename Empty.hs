module Empty where
import Prelude hiding (init)
import System.Exit
import Graphics.UI.GLUT
import Framework

defaults = do
  return ()

init = do
  return ()

display = do
  swapBuffers

idle = do
  return ()

reshape size = do
  return ()

keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _key         _state _modifiers _position = return ()

main = framework defaults init display idle reshape keyboard
