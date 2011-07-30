-- Written by Aur Saraf
-- 
-- Released to the public domain
-- 
-- Closely follows tutorial on:
--   http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2:-Hello-World:-The-Slideshow.html
-- 
-- Chapter 2 done

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
                         
                       , fadeFactorU :: UniformLocation
                       , texturesU :: [UniformLocation]
                       , positionA :: AttribLocation
                       }

data Resources = Resources { vertexBuffer :: BufferObject
                           , elementBuffer :: BufferObject
                           , textures :: [TextureObject]
                           , shaders :: Shaders
                           , fadeFactor :: GLfloat
                           }

defaults = do
  return ()

init resources = do
  r <- makeResources
  resources $= r

vertexData :: [GLfloat]
vertexData = [
   -1, -1
  ,-1,  1
  , 1, -1
  , 1,  1
  ]
elementData :: [GLuint]
elementData = [0, 1, 2, 3]
makeResources = Resources
                <$> makeBuffer ArrayBuffer vertexData
                <*> makeBuffer ElementArrayBuffer elementData
                <*> mapM makeTexture ["hello1.tga", "hello2.tga"]
                <*> makeShaders
                <*> pure 0.0

makeTexture filename = do
  (width, height, pixels) <- readTGA filename
  texture <- loadTexture $ texInfo width height TexBGR pixels
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  return texture

makeShaders = do
  vs <- loadShader "hello-gl.v.glsl"
  fs <- loadShader "hello-gl.f.glsl"
  p <- linkShaderProgram [vs] [fs]
  Shaders vs fs p
    <$> get (uniformLocation p "fade_factor")
    <*> mapM (get . uniformLocation p) ["textures[0]", "textures[1]"]
    <*> get (attribLocation p "position")

display resources = do
  r <- get resources
  currentProgram $= Just (program (shaders r))
  uniform (fadeFactorU (shaders r)) $= Index1 (fadeFactor r)
  
  let [t0, t1] = textures r
      [tu0, tu1] = texturesU (shaders r)
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just t0
  uniform tu0 $= Index1 (0 :: GLint)
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just t1
  uniform tu1 $= Index1 (1 :: GLint)
  
  bindBuffer ArrayBuffer $= Just (vertexBuffer r)
  let stride = 0
      position = positionA (shaders r)
  vertexAttribPointer position $= (ToFloat, VertexArrayDescriptor 2 Float stride offset0)
  vertexAttribArray position $= Enabled
  
  bindBuffer ElementArrayBuffer $= Just (elementBuffer r)
  drawElements TriangleStrip 4 UnsignedInt offset0
  
  vertexAttribArray position $= Disabled
  
  swapBuffers

idle resources = do
  t <- get elapsedTime
  r <- get resources
  resources $= r {fadeFactor=sin (fromIntegral t * 0.001) * 0.5 + 0.5}
  postRedisplay Nothing

reshape size = do
  return ()

keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _key         _state _modifiers _position = return ()

main = do
  resources <- newIORef $ undefined
  framework defaults (init resources) (display resources) (idle resources) reshape keyboard
