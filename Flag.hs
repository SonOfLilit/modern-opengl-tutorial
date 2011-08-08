-- Written by Aur Saraf
-- 
-- Released to the public domain
-- 
-- Closely follows tutorial on:
--   http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-4:-Rendering-a-Dynamic-3D-Scene-with-Phong-Shading.html
-- 
-- First compile

module Main where
import Prelude hiding (init)
import System.Exit
import Data.Monoid
import Control.Applicative
import Data.IORef
import Foreign.Ptr
import Unsafe.Coerce
import Foreign.Storable
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import Graphics.GLUtil
import TGA
import Framework
import Matrix

data MeshVertex = MeshVertex { vPosition :: !(Vertex3 GLfloat)
                             , vNormal   :: !(Normal3 GLfloat)
                             , vTexcoord :: !(TexCoord2 GLfloat)
                             , vShininess :: !GLfloat
                             , vSpecular :: !(Vector4 GLubyte)
                             }
                  deriving (Eq, Ord, Show, Read)

_meshVertex = MeshVertex undefined undefined undefined undefined undefined
_meshVertexSizes = (4*4, 4*4, 2*4, 2*4, 4*4)
instance Storable MeshVertex where
   sizeOf ~(MeshVertex _ _ _ _ _) = let (a, b, c, d, e) = _meshVertexSizes
                                    in a + b + c + d + e
   alignment ~(_) = 16
   peek ptr = let (a', b', c', d', _) = _meshVertexSizes
                  a = a'
                  b = a' + b'
                  c = b + c'
                  d = c + d'
              in MeshVertex
                  <$> peek (castPtr ptr               :: Ptr (Vertex3 GLfloat))
                  <*> peek (castPtr (ptr `plusPtr` a) :: Ptr (Normal3 GLfloat))
                  <*> peek (castPtr (ptr `plusPtr` b) :: Ptr (TexCoord2 GLfloat))
                  <*> peek (castPtr (ptr `plusPtr` c) :: Ptr (GLfloat))
                  <*> peek (castPtr (ptr `plusPtr` d) :: Ptr (Vector4 GLubyte))
   poke ptr (MeshVertex u v w x y) = do
     let (a', b', c', d', _) = _meshVertexSizes
         a = a'
         b = a' + b'
         c = b + c'
         d = c + d'
     poke (castPtr ptr               :: Ptr (Vertex3 GLfloat)) u
     poke (castPtr (ptr `plusPtr` a) :: Ptr (Normal3 GLfloat)) v
     poke (castPtr (ptr `plusPtr` b) :: Ptr (TexCoord2 GLfloat)) w
     poke (castPtr (ptr `plusPtr` c) :: Ptr (GLfloat))         x
     poke (castPtr (ptr `plusPtr` d) :: Ptr (Vector4 GLubyte)) y

data MeshData = MeshData { meshVertices :: [MeshVertex]
                         , meshElements :: [GLuint]
                         , meshTexturePath :: FilePath
                         }

data Shaders = Shaders { vertexShader :: VertexShader
                       , fragmentShader :: FragmentShader
                       , program :: Program
                         
                       , uniforms :: Uniforms
                       , attribs :: Attribs
                       }

data Uniforms = Uniforms { timerU :: UniformLocation
                         , textureU :: UniformLocation
                         , projectionMatrixU :: UniformLocation
                         , modelViewMatrixU :: UniformLocation
                         }

data Attribs = Attribs { positionA :: AttribLocation
                       , normalA :: AttribLocation
                       , texcoordA :: AttribLocation
                       , shininessA :: AttribLocation
                       , specularA :: AttribLocation
                       }

data Mesh = Mesh { vertexBuffer :: BufferObject
                 , elementBuffer :: BufferObject
                 , elementCount :: NumArrayIndices
                 , textureO :: TextureObject
                 }

data Resources = Resources { flagMesh :: Mesh
                           , backgroundMesh :: Mesh
                           , shaders :: Shaders
                           , timer :: GLfloat
                           , eyeOffset :: (GLfloat, GLfloat)
                           , rWindowSize :: Size
                           , projectionMatrix :: Matrix4x4
                           , modelViewMatrix :: Matrix4x4
                           }

defaults = do
  initialWindowSize $= Size 640 480

init resources args = do
  let (vertexShaderPath, fragmentShaderPath) = case args of
        [vp, fp] -> (vp, fp)
        [fp] -> ("flag.v.glsl", fp)
        [] -> ("flag.v.glsl", "flag.f.glsl")
  r <- makeResources vertexShaderPath fragmentShaderPath
  resources $= r
  
  motionCallback $= Just (drag resources . Just)

data MeshPart = MeshPart [MeshVertex] [GLuint]
instance Monoid MeshPart where
  mempty = MeshPart [] []
  MeshPart av ae `mappend` MeshPart bv be = 
    MeshPart (av ++ bv) (ae ++ be')
      where be' = map (+avlen) be
            avlen = fromIntegral . toInteger . length $ av

flagMeshData = MeshData vertexData elementData "flag.tga"
backgroundMeshData =
  let rectangleVertices (x, y, z) (x', y', z') (a, b) (a', b') =
        let x0 = Vertex3 x y z
            x1 = Vertex3 x' y z
            x2 = Vertex3 x' y z'
            x3 = Vertex3 x y z'
            normalV = normalizeV3 $ (x2 `subV3` x0) `crossV3` (x1 `subV3` x0)
            t0 = TexCoord2 a b
            t1 = TexCoord2 a' b
            t2 = TexCoord2 a' b'
            t3 = TexCoord2 a b'
        in [ MeshVertex x0 normalV t0 0.0 (Vector4 0 0 0 0)
           , MeshVertex x1 normalV t1 0.0 (Vector4 0 0 0 0) 
           , MeshVertex x2 normalV t2 0.0 (Vector4 0 0 0 0) 
           , MeshVertex x3 normalV t3 0.0 (Vector4 0 0 0 0) 
           ]
      rectangleElements = [ 0, 1, 2
                          , 3, 1, 2]
      groundY = -1.0
      groundMesh = MeshPart (
        rectangleVertices (-0.875, groundY, -2.45) (1.875, groundY, 0.20)
                          (0.03125, 0.0078125) (0.515625, 0.9921875))
        rectangleElements
      MeshPart vertexData elementData = mconcat [groundMesh]
  in MeshData vertexData elementData "background.tga"

vertexData :: [MeshVertex]
vertexData = [
    MeshVertex (Vertex3 0 0 0) (Normal3 0 0 1) (TexCoord2 0 0) 1.0 (Vector4 1 0 0 255)
  , MeshVertex (Vertex3 0 1 0) (Normal3 0 0 1) (TexCoord2 0 1) 1.0 (Vector4 0 1 0 255)
  , MeshVertex (Vertex3 1 0 0) (Normal3 0 0 1) (TexCoord2 1 1) 1.0 (Vector4 0 0 1 255)
  ]
elementData :: [GLuint]
elementData = [0, 1, 2]

makeResources :: FilePath -> FilePath -> IO Resources
makeResources vertexShaderPath fragmentShaderPath =
  let initialTime = 0.0
      initialEyeOffset = (0.0, 0.0)
  in Resources
     <$> makeMesh flagMeshData
     <*> makeMesh backgroundMeshData
     <*> makeShaders vertexShaderPath fragmentShaderPath
     <*> pure initialTime
     <*> pure initialEyeOffset
     <*> get initialWindowSize
     <*> (get initialWindowSize >>= (pure . calculateProjectionMatrix) :: IO Matrix4x4)
     <*> (pure (calculateModelViewMatrix initialEyeOffset) :: IO Matrix4x4)

calculateProjectionMatrix :: Size -> Matrix4x4
calculateProjectionMatrix (Size x y) =
  let wf = fromIntegral x
      hf = fromIntegral y
      projectionFovRatio = 0.7
      projectionNearPlane = 0.0625
      projectionFarPlane = 256.0
      r_xy_factor = min wf hf * 1.0 / projectionFovRatio
      r_x = r_xy_factor / wf
      r_y = r_xy_factor / hf
      r_zw_factor = 1.0 / (projectionFarPlane - projectionNearPlane)
      r_z = (projectionNearPlane + projectionFarPlane) * r_zw_factor
      r_w = -2.0 * projectionNearPlane * projectionFarPlane * r_zw_factor
  in [ [r_x, 0.0, 0.0, 0.0]
     , [0.0, r_y, 0.0, 0.0] 
     , [0.0, 0.0, r_z, 1.0]
     , [0.0, 0.0, r_w, 0.0]
     ]

calculateModelViewMatrix :: (GLfloat, GLfloat) -> Matrix4x4
calculateModelViewMatrix (offsetX, offsetY) =
  let baseEyePosition = (0.5, -0.25, -1.25)
      (baseX, baseY, baseZ) = baseEyePosition
  in [ [ 1.0, 0.0, 0.0, 0.0]
     , [ 0.0, 1.0, 0.0, 0.0]
     , [ 0.0, 0.0, 1.0, 0.0]
     , [(-baseX) - offsetX, (-baseY) - offsetY, -baseZ, 1.0]
     ]

makeTexture filename = do
  (width, height, pixels) <- readTGA filename
  texture <- loadTexture $ texInfo width height TexBGR pixels
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  return texture

makeShaders :: FilePath -> FilePath -> IO Shaders
makeShaders vertexShaderPath fragmentShaderPath = do
  vs <- loadShader vertexShaderPath
  fs <- loadShader fragmentShaderPath
  p <- linkShaderProgram [vs] [fs]
  let uniforms = Uniforms
                 <$> get (uniformLocation p "timer")
                 <*> get (uniformLocation p "texture")
                 <*> get (uniformLocation p "p_matrix")
                 <*> get (uniformLocation p "mv_matrix")
      attribs = Attribs
                <$> get (attribLocation p "position")
                <*> get (attribLocation p "normal")
                <*> get (attribLocation p "texcoord")
                <*> get (attribLocation p "shininess")
                <*> get (attribLocation p "specular")

  Shaders vs fs p
    <$> uniforms
    <*> attribs

makeMesh :: MeshData -> IO Mesh
makeMesh (MeshData vertices elements texturePath) = Mesh
    <$> makeBuffer ArrayBuffer vertices
    <*> makeBuffer ElementArrayBuffer elements
    <*> pure (fromIntegral . toInteger $ length elements)
    <*> makeTexture texturePath

display resources = do
  r <- get resources
  let s = shaders r
      u = uniforms s
      a = attribs s
  
  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer, DepthBuffer]

  polygonMode $= (Fill, Line)
  
  currentProgram $= Just (program s)
  uniform (timerU u) $= Index1 (timer r)
  activeTexture $= TextureUnit 0
  
  uniform (textureU u) $= Index1 (0 :: GLint)

  setUniformMatrix4x4 (projectionMatrixU u) (projectionMatrix r)
  setUniformMatrix4x4 (modelViewMatrixU u) (modelViewMatrix r)

  vertexAttribArray (positionA a)  $= Enabled
  vertexAttribArray (normalA a)    $= Enabled
  vertexAttribArray (texcoordA a)  $= Enabled
  vertexAttribArray (shininessA a) $= Enabled
  vertexAttribArray (specularA a)  $= Enabled
  
  renderMesh r (backgroundMesh r)
--  renderMesh r (flagMesh r)
  
  vertexAttribArray (positionA a)  $= Disabled
  vertexAttribArray (normalA a)    $= Disabled
  vertexAttribArray (texcoordA a)  $= Disabled
  vertexAttribArray (shininessA a) $= Disabled
  vertexAttribArray (specularA a)  $= Disabled

  swapBuffers


setUniformMatrix4x4 :: UniformLocation -> Matrix4x4 -> IO ()
setUniformMatrix4x4 location value =
  withMatrix4x4 value (\order ptr -> 
                        glUniformMatrix4fv location' 1 false (castPtr ptr :: Ptr GLfloat))
    where location' = unsafeCoerce location :: GLint
          false = 0
  
renderMesh :: Resources -> Mesh -> IO ()
renderMesh r mesh = do
  let s = shaders r
      as = attribs s
      t = textureO mesh
  textureBinding Texture2D $= Just t

  let setVap a conversion size dataType offset = 
        vertexAttribPointer (a as) $= (conversion, VertexArrayDescriptor size dataType stride (wordPtrToPtr . fromIntegral $ offset))
      stride = (fromIntegral . toInteger) $ sizeOf (undefined :: MeshVertex)
      (a', b', c', d', _) = _meshVertexSizes
      [a, b, c, d] = map (fromIntegral . toInteger) [a', b', c', d']

  setVap positionA  ToFloat 3 Float 0
  setVap normalA    ToFloat 3 Float a
  setVap texcoordA  ToFloat 2 Float (a+b)
  setVap shininessA ToFloat 1 Float (a+b+c)
  setVap specularA  ToFloat 4 UnsignedByte (a+b+c+d)
  
  bindBuffer ArrayBuffer $= Just (vertexBuffer mesh)

  bindBuffer ElementArrayBuffer $= Just (elementBuffer mesh)
  drawElements Triangles (elementCount mesh) UnsignedInt offset0
  
idle :: IORef Resources -> IO ()
idle resources = do
  t <- get elapsedTime
  r <- get resources
  resources $= r {timer=fromIntegral t * 0.001}
  postRedisplay Nothing

reshape resources size = do
  r <- get resources
  resources $= r { rWindowSize=size
                 , projectionMatrix=calculateProjectionMatrix size
                 }
  viewport $= (Position 0 0, size)

keyboardMouse _resources (Char '\27')             Down _ _ = exitWith ExitSuccess
keyboardMouse resources  (MouseButton LeftButton) Up _ _ = drag resources Nothing
keyboardMouse _resources _key                     _state _modifiers _position = return ()

drag resources mposition = do
  r <- get resources
  let Size w h = rWindowSize r
      newEyeOffset = case mposition of
        Just (Position x y) -> (fromIntegral x / fromIntegral w - 0.5, 
                                fromIntegral y / fromIntegral h + 0.5)
        Nothing -> (0, 0)
        
  resources $= r { eyeOffset=newEyeOffset 
                 , modelViewMatrix=calculateModelViewMatrix newEyeOffset
                 }
  postRedisplay Nothing

main :: IO ()
main = do
  -- TODO: Write mesh to file
  resources <- newIORef $ undefined
  framework defaults (init resources) (display resources) (idle resources) (reshape resources) (keyboardMouse resources)
