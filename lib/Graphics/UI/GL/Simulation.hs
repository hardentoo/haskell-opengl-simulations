-- easily extensible GL simulation application with reasonable defaults
module Graphics.UI.GL.Simulation (
    module Data.GL,
    module Graphics.UI.GLUT,
    Simulation(..), SimWindow(..), Camera(..), KeySet
) where
import Graphics.UI.GLUT hiding (Matrix,newMatrix)
import Data.GL
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef,newIORef)
import Control.Monad (when,forever)
import qualified Data.Set as Set
import Control.Concurrent (forkIO,threadDelay)

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

type KeySet = Set.Set Key
data InputState = InputState {
    keySet :: KeySet,
    mousePos :: (GLint,GLint)
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
    
    initCamera :: a -> IO Camera
    initCamera sim = do
        m <- newMatrix $ do
            rotate 90.0 $ vector3f 1 0 0 -- z-up
            translate $ vector3f 0 (-4) 2
        return $ Camera {
            cameraFOV = 70,
            cameraNear = 0.1,
            cameraFar = 100000,
            cameraMatrix = m
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
    
    reshape :: a -> Camera -> ReshapeCallback
    reshape sim cam size@(Size w h) = do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        let
            fov = cameraFOV cam
            near = cameraNear cam
            far = cameraFar cam
            w' = fromIntegral w
            h' = fromIntegral h
        perspective fov (w' / h') near far
        matrixMode $= Modelview 0
    
    navigation :: a -> InputState -> GLmatrix GLdouble -> IO (GLmatrix GLdouble)
    navigation sim input mat = do
        let
            keys = keySet input
            pos = mousePos input
        return $ foldl (\m k -> keyf k m) mat $ Set.elems keys
            where
                dv = 0.1
                keyf (Char 'w') = mTranslate (vector3d 0 dv 0)
                keyf (Char 's') = mTranslate (vector3d 0 (-dv) 0)
                keyf (Char 'a') = mTranslate (vector3d dv 0 0)
                keyf (Char 'd') = mTranslate (vector3d (-dv) 0 0)
                keyf _ = id
    
    keyboard :: a -> KeyboardMouseCallback
    keyboard sim key keyState modifiers pos = return ()
    
    mouseMove :: a -> MotionCallback
    mouseMove sim pos = return ()
    
    runSimulation :: a -> IO ()
    runSimulation sim = do
        (_, argv) <- getArgsAndInitialize
        initWindow sim
        initDisplay sim
        
        simRef <- newIORef sim
        inputRef <- newIORef $ InputState {
            keySet = Set.empty,
            mousePos = (0,0)
        }
        
        camera <- initCamera sim
        cameraRef <- newIORef camera
        reshapeCallback $= Just (reshape sim camera)
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
        
        -- bind passive motion callback for mouse movement
        (passiveMotionCallback $=) . Just $ \pos -> do
            let Position posX posY = pos
            inputRef $~ \i -> i { mousePos = (posX,posY) }
            sim <- get simRef
            mouseMove sim pos
        
        -- bind keyboard callback
        (keyboardMouseCallback $=) . Just $
            \key keyState modifiers pos -> do
                when (key == Char '\27') leaveMainLoop -- esc
                -- update key set
                inputRef $~ \i -> i {
                    keySet = ($ keySet i) $ case keyState of
                        Down -> Set.insert key
                        Up -> Set.delete key
                }
                -- run user callback
                sim <- get simRef
                keyboard sim key keyState modifiers pos
        
        -- navigation gets its own thread with regular updates
        forkIO $ forever $ do
            t' <- elapsed $ do
                sim <- get simRef
                cam <- get cameraRef
                input <- get inputRef
                mat <- navigation sim input (cameraMatrix cam)
                cameraRef $= cam { cameraMatrix = mat }
            let t = max 0.0 (0.01 - t')
            -- ~(1/100) seconds between updates
            threadDelay $ floor (t * 1e6)
        
        displayCallback $= do
            sim <- get simRef
            clearColor $= (winBG $ window sim)
            clear [ ColorBuffer, DepthBuffer ]
            
            loadIdentity
            cam <- get cameraRef
            multMatrix $ cameraMatrix cam
            
            (simRef $=) =<< display sim
            
            flush
            swapBuffers
            postRedisplay Nothing
        
        mainLoop

-- elapsed time to run the supplied action in seconds
elapsed :: Floating a => IO () -> IO a
elapsed m = do
    t1 <- getCurrentTime
    m
    t2 <- getCurrentTime
    return $ fromRational $ toRational $ diffUTCTime t1 t2
