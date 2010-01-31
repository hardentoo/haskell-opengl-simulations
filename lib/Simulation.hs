-- Simulation.hs - easily extensible, reasonable defaults
module Simulation where
import Graphics.UI.GLUT
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef,newIORef)
import Control.Monad (when)
import qualified Data.Set as Set


data SimWindow = SimWindow {
    winTitle :: String,
    winSize :: (GLsizei,GLsizei),
    winPos :: (GLint,GLint),
    winBG :: Color4 GLclampf
}

data Camera = Camera {
    cameraFOV :: GLdouble,
    cameraNear :: GLdouble,
    cameraFar :: GLdouble,
    cameraMatrix :: GLmatrix GLdouble
}

buildMatrix :: IO () -> IO (GLmatrix GLdouble)
buildMatrix matM = preservingMatrix $ do
    loadIdentity
    matM
    get $ matrix Nothing

class Simulation a where
    display :: a -> IO a
    display = return
    
    window :: a -> SimWindow
    window = const $ SimWindow {
        winTitle = "simulation",
        winSize = (512,512),
        winPos = (0,0),
        winBG = Color4 0.2 0.2 0.2 1
    }
    
    camera :: a -> Camera
    camera = const $ Camera {
        cameraFOV = 70,
        cameraNear = 0.1,
        cameraFar = 100000,
        cameraMatrix = unsafePerformIO $ buildMatrix $ do
            translate $ vec3f 0 (-10) 5
    }
    
    initModes :: a -> [ DisplayMode ]
    initModes = const [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    
    initWindow :: a -> IO ()
    initWindow sim = do
        let win = window sim
        initialDisplayMode $= initModes sim
        initialWindowSize $= (uncurry Size $ winSize win)
        initialWindowPosition $= (uncurry Position $ winPos win)
        createWindow $ winTitle win
        return ()
    
    initDisplay :: a -> IO ()
    initDisplay sim = do
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        shadeModel $= Flat
        depthMask $= Enabled
        depthFunc $= Just Lequal
        pointSmooth $= Enabled
        lineSmooth $= Enabled
        lighting $= Disabled
        texture Texture2D $= Enabled
    
    reshape :: a -> ReshapeCallback
    reshape sim size@(Size w h) = do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        let
            cam = camera sim
            fov = cameraFOV cam
            near = cameraNear cam
            far = cameraFar cam
            w' = fromIntegral w
            h' = fromIntegral h
        perspective fov (w' / h') near far
        matrixMode $= Modelview 0
    
    keysRef :: a -> IORef (Set.Set Key)
    keysRef sim = unsafePerformIO
        $ newIORef (Set.empty :: Set.Set Key)
    
    keyboard :: a -> KeyboardMouseCallback
    keyboard sim key keyState modifiers pos = do
        when (key == Char '\27') leaveMainLoop
        (keysRef sim $~) $ case keyState of
            Down -> Set.insert key
            Up -> Set.delete key
    
    runSimulation :: a -> IO ()
    runSimulation sim = do
        (_, argv) <- getArgsAndInitialize
        initWindow sim
        initDisplay sim
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
        (keyboardMouseCallback $=) . Just $
            \key keyState modifiers pos -> do
                when (key == Char '\27') leaveMainLoop
                keyboard sim key keyState modifiers pos
        
        reshapeCallback $= Just (reshape sim)
        
        simRef <- newIORef sim
        displayCallback $= do
            sim <- get simRef
            clearColor $= (winBG $ window sim)
            clear [ ColorBuffer, DepthBuffer ]
            
            (simRef $=) =<< display sim
            
            flush
            swapBuffers
            postRedisplay Nothing
        
        mainLoop
 
vec3f :: Real a => a -> a -> a -> Vector3 GLfloat
vec3f x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational
