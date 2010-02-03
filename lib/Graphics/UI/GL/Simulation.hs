-- easily extensible GL simulation application with reasonable defaults
module Graphics.UI.GL.Simulation (
    module Data.GL,
    module Graphics.UI.GLUT,
    Simulation(..), SimWindow(..), Camera(..), KeySet,
    runAtFPS, runWithFPS, elapsed, exitSimulation
) where
import Graphics.UI.GLUT hiding (Matrix,newMatrix)
import Data.GL
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef,newIORef)

import Control.Monad (when,forever)
import Control.Arrow (first)
import Control.Applicative ((<$>))
import Data.Maybe (isJust,fromJust)

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
    cameraPos :: Vector3 GLdouble,
    cameraYaw :: GLdouble,
    cameraPitch :: GLdouble
    -- no roll
}

type KeySet = Set.Set Key
data InputState = InputState {
    keySet :: KeySet,
    mousePos :: (GLint,GLint),
    prevMousePos :: (GLint,GLint)
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
    
    initCamera :: a -> Camera
    initCamera sim = Camera {
        cameraFOV = 60,
        cameraNear = 0.1,
        cameraFar = 100000,
        cameraPos = vector3d 0 4 (-2),
        cameraYaw = 0.0,
        cameraPitch = 0.0
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
    
    initSimulation :: a -> IO a
    initSimulation = return
    
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
    
    navigate :: a -> InputState -> Camera -> IO Camera
    navigate sim input cam = return cam'
        where
            keys = keySet input
            pos = mousePos input
            prevPos = prevMousePos input
            
            cam' = foldl (\m k -> keyf k m) cam $ Set.elems keys
            
            dt = 0.1
            drx = -0.1 * (fromIntegral $ fst pos - fst prevPos)
            dry = 0.1 * (fromIntegral $ snd pos - snd prevPos)
            
            fpos f c = c { cameraPos = f $ cameraPos c }
            fangle f g c = c {
                cameraPitch = f $ cameraPitch c,
                cameraYaw = g $ cameraYaw c
            }
            
            keyf :: Key -> Camera -> Camera
            keyf (Char 'w') = fpos (<+> (vector3d 0 dt 0)) -- forward
            keyf (Char 's') = fpos (<+> (vector3d 0 (-dt) 0)) -- back
            keyf (Char 'a') = fpos (<+> (vector3d dt 0 0)) -- strafe left
            keyf (Char 'd') = fpos (<+> (vector3d (-dt) 0 0)) -- strafe right
            keyf (Char 'q') = fpos (<+> (vector3d 0 0 dt)) -- up
            keyf (Char 'e') = fpos (<+> (vector3d 0 0 (-dt))) -- down
            keyf (MouseButton LeftButton) = fangle (+ dry) (+ drx)
            keyf _ = id
    
    keyboard :: a -> KeyboardMouseCallback
    keyboard sim key keyState modifiers pos = return ()
    
    mouseMove :: a -> MotionCallback
    mouseMove sim pos = return ()
    
    runSimulation :: a -> IO ()
    runSimulation sim' = do
        (_, argv) <- getArgsAndInitialize
        initWindow sim'
        initDisplay sim'
        sim <- initSimulation sim'
        
        simRef <- newIORef sim
        inputRef <- newIORef $ InputState {
            keySet = Set.empty,
            mousePos = (0,0),
            prevMousePos = (0,0)
        }
        
        let camera = initCamera sim
        cameraRef <- newIORef camera
        reshapeCallback $= Just (reshape sim camera)
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
        
        -- bind passive motion callback for mouse movement polling
        (passiveMotionCallback $=) . Just $ \pos -> do
            let Position posX posY = pos
            inputRef $~ \i -> i { mousePos = (posX,posY) }
            sim <- get simRef
            mouseMove sim pos
        
        -- more mouse polling (when buttons are down)
        (motionCallback $=) . Just $ \pos -> do
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
        forkIO $ forever $ runAtFPS 100 $ do
            sim <- get simRef
            cam <- get cameraRef
            input <- get inputRef
            
            cam' <- navigate sim input cam
            cameraRef $= cam'
            inputRef $~ \i -> i { prevMousePos = mousePos input }
        
        -- run display callback with helper stuff
        displayCallback $= do
            sim <- get simRef
            clearColor $= (winBG $ window sim)
            clear [ ColorBuffer, DepthBuffer ]
            
            matrixMode $= Modelview 0
            loadIdentity
            
            rotate 90.0 $ vector3f 1 0 0 -- z-up
            
            cam <- get cameraRef
            translate $ cameraPos cam
            rotate (cameraYaw cam) $ vector3d 0 0 1
            rotate (cameraPitch cam) $ vector3d 1 0 0
            
            (simRef $=) =<< display sim
            
            flush
            swapBuffers
            postRedisplay Nothing
        
        mainLoop

-- elapsed time to run the supplied action in seconds
elapsed :: Floating a => IO b -> IO (a,b)
elapsed m = do
    t1 <- getCurrentTime
    result <- m
    t2 <- getCurrentTime
    let t = fromRational $ toRational $ diffUTCTime t1 t2
    return (t,result)

-- run an action at a number of frames per second, if possible
-- if not enough time has elapsed, waits until fps is achieved
runAtFPS :: (Floating a, RealFrac a) => a -> IO b -> IO b
runAtFPS fps m = do
    (t',result) <- elapsed m
    let t = recip fps - t'
    when (t > 0) $ threadDelay $ floor (t * 1e6)
    return result

-- run an action, returning the frames per second along with the result
runWithFPS :: (Floating a, RealFrac a) => IO b -> IO (a,b)
runWithFPS = (first recip <$>) . elapsed

exitSimulation :: IO ()
exitSimulation = do
    leaveMainLoop
    win <- get currentWindow
    when (isJust win) $ destroyWindow (fromJust win)
