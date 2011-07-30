-- Written by Aur Saraf
-- 
-- Released to the public domain
-- 
-- Closely follows tutorial on:
--   http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2:-Hello-World:-The-Slideshow.html
-- 
-- Perspective projections is supposed to work but doesn't

module Tutorial where
import Prelude hiding (init)
import System.Exit
import Control.Applicative
import Data.IORef
import Graphics.UI.GLUT
import Graphics.GLUtil
import TGA
import Framework

data Shaders = Shaders { vertexShader :: VertexShader
                       , fragmentShader :: FragmentShader
                       , program :: Program
                         
                       , timerU :: UniformLocation
                       , texturesU :: [UniformLocation]
                       , positionA :: AttribLocation
                       }

data Resources = Resources { vertexBuffer :: BufferObject
                           , elementBuffer :: BufferObject
                           , textures :: [TextureObject]
                           , shaders :: Shaders
                           , timer :: GLfloat
                           }

defaults = do
  return ()

init resources args = do
  let vertexShaderPath = case args of
        [p] -> p
        [] -> "hello-gl.v.glsl"
  r <- makeResources vertexShaderPath
  resources $= r

vertexData :: [GLfloat]
vertexData = [
   -1, -1, 0, 1
  ,-1,  1, 0, 1
  , 1, -1, 0, 1
  , 1,  1, 0, 1
  ]
elementData :: [GLuint]
elementData = [0, 1, 2, 3]
makeResources vertexShaderPath = Resources
                                 <$> makeBuffer ArrayBuffer vertexData
                                 <*> makeBuffer ElementArrayBuffer elementData
                                 <*> mapM makeTexture ["hello1.tga", "hello2.tga"]
                                 <*> makeShaders vertexShaderPath
                                 <*> pure 0.0

makeTexture filename = do
  (width, height, pixels) <- readTGA filename
  texture <- loadTexture $ texInfo width height TexBGR pixels
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  return texture

makeShaders vertexShaderPath = do
  vs <- loadShader vertexShaderPath
  fs <- loadShader "hello-gl.f.glsl"
  p <- linkShaderProgram [vs] [fs]
  Shaders vs fs p
    <$> get (uniformLocation p "timer")
    <*> mapM (get . uniformLocation p) ["textures[0]", "textures[1]"]
    <*> get (attribLocation p "position")

display resources = do
  clearColor $= Color4 1 1 1 1
  clear [ColorBuffer]
  
  r <- get resources
  currentProgram $= Just (program (shaders r))
  uniform (timerU (shaders r)) $= Index1 (timer r)
  
  let [t0, t1] = textures r
      [tu0, tu1] = texturesU (shaders r)
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just t0
  uniform tu0 $= Index1 (0 :: GLint)
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just t1
  uniform tu1 $= Index1 (1 :: GLint)
  
  bindBuffer ArrayBuffer $= Just (vertexBuffer r)
  let stride = 0  -- defaults to tight packing
      position = positionA (shaders r)
  vertexAttribPointer position $= (ToFloat, VertexArrayDescriptor 4 Float stride offset0)
  vertexAttribArray position $= Enabled
  
  bindBuffer ElementArrayBuffer $= Just (elementBuffer r)
  drawElements TriangleStrip 4 UnsignedInt offset0
  
  vertexAttribArray position $= Disabled
  
  swapBuffers

idle resources = do
  t <- get elapsedTime
  r <- get resources
  resources $= r {timer=fromIntegral t * 0.001}
  postRedisplay Nothing

reshape size = do
  return ()

keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _key         _state _modifiers _position = return ()

main = do
  resources <- newIORef $ undefined
  framework defaults (init resources) (display resources) (idle resources) reshape keyboard
