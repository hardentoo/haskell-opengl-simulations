-- easily extensible GL simulation application with reasonable defaults
module Graphics.UI.GL.Simulation where
import Graphics.UI.GLUT
import Data.Matrix.GL (vector3f,buildMatrix)

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
            rotate 90.0 $ vector3f 1 0 0 -- z-up
            translate $ vector3f 0 (-4) 2
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
    
    keySetRef :: a -> IORef (Set.Set Key)
    keySetRef sim = unsafePerformIO
        $ newIORef (Set.empty :: Set.Set Key)
    
    navigation :: a -> GLmatrix GLdouble -> IO (GLmatrix GLdouble)
    navigation sim gl = do
        keys <- get $ keySetRef sim
        when (Char 'w' `Set.member` keys) $ do
            putStrLn "w!"
        return gl
    
    keyboard :: a -> KeyboardMouseCallback
    keyboard sim key keyState modifiers pos = return ()
    
    runSimulation :: a -> IO ()
    runSimulation sim = do
        (_, argv) <- getArgsAndInitialize
        initWindow sim
        initDisplay sim
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
        (keyboardMouseCallback $=) . Just $
            \key keyState modifiers pos -> do
                when (key == Char '\27') leaveMainLoop -- esc
                -- update keySetRef state
                (keySetRef sim $~) $ case keyState of
                    Down -> Set.insert key
                    Up -> Set.delete key
                -- run user callback
                keyboard sim key keyState modifiers pos
        
        reshapeCallback $= Just (reshape sim)
        
        simRef <- newIORef sim
        displayCallback $= do
            sim <- get simRef
            clearColor $= (winBG $ window sim)
            clear [ ColorBuffer, DepthBuffer ]
            
            loadIdentity
            multMatrix $ cameraMatrix $ camera sim
            
            (simRef $=) =<< display sim
            
            flush
            swapBuffers
            postRedisplay Nothing
        
        mainLoop
