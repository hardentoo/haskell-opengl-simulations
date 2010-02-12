-- easily extensible GLUT simulation application with reasonable defaults
module Graphics.UI.GLUT.Simulation (
    module Data.GL,
    module Graphics.UI.GLUT,
    Simulation(..), SimWindow(..), Camera(..), KeySet,
    runAtFPS, runWithFPS, elapsed, exitSimulation
) where
import Graphics.UI.GLUT hiding (Matrix,newMatrix)
import Data.GL
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM (STM,atomically)
import Control.Concurrent.STM.TMVar

import Control.Monad (when,forever)
import Control.Monad.Trans (liftIO)
import Control.Arrow (first)
import Control.Applicative ((<$>))
import Data.Maybe (isJust,fromJust)

import qualified Data.Set as Set
import Control.Concurrent (forkIO,threadDelay)

-- STM TMVar versions of Data.StateVar shorthand operators
($$~) :: TMVar a -> (a -> a) -> STM ()
tm $$~ f = putTMVar tm . f =<< takeTMVar tm

($$=) :: TMVar a -> a -> STM ()
tm $$= v = takeTMVar tm >> putTMVar tm v

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
} deriving Show

type KeySet = Set.Set Key
data InputState = InputState {
    keySet :: KeySet,
    mousePos :: (GLint,GLint),
    prevMousePos :: (GLint,GLint)
}

class Simulation a where
    preDisplay :: a -> IO a
    preDisplay = return
    
    display :: a -> IO a
    display = return
    
    postDisplay :: a -> IO a
    postDisplay = return
    
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
            rotate 30 $ vector3f 1 0 0
            translate $ vector3f 0 (-2) (-4)
        return $ Camera {
            cameraFOV = 60,
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
    
    initSimulation :: a -> IO a
    initSimulation = return
    
    projection :: a -> Camera -> IO ()
    projection sim cam = do
        matrixMode $= Projection
        loadIdentity
        Size w h <- get windowSize
        let
            fov = cameraFOV cam
            near = cameraNear cam
            far = cameraFar cam
            w' = fromIntegral w
            h' = fromIntegral h
        perspective fov (w' / h') near far
    
    reshape :: a -> Camera -> ReshapeCallback
    reshape sim cam size@(Size w h) = do
        viewport $= (Position 0 0, size)
        projection sim cam
        matrixMode $= Modelview 0
    
    navigate :: a -> InputState -> Camera -> Camera
    navigate sim input cam = cam { cameraMatrix = mat' }
        where
            keys = keySet input
            pos = mousePos input
            prevPos = prevMousePos input
            
            mat = cameraMatrix cam
            mat' = foldl (\m k -> keyf k m) mat $ Set.elems keys
            
            dt = 0.05
            drx = 0.05 * (fromIntegral $ fst pos - fst prevPos)
            dry = -0.05 * (fromIntegral $ snd pos - snd prevPos)
            
            keyf :: Key -> GLmatrix GLdouble -> GLmatrix GLdouble
            keyf (Char 'w') = mTranslate (vector3d 0 0 dt) -- forward
            keyf (Char 's') = mTranslate (vector3d 0 0 (-dt)) -- back
            keyf (Char 'a') = mTranslate (vector3d dt 0 0) -- strafe left
            keyf (Char 'd') = mTranslate (vector3d (-dt) 0 0) -- strafe right
            keyf (Char 'q') = mTranslate (vector3d 0 (-dt) 0) -- up
            keyf (Char 'z') = mTranslate (vector3d 0 dt 0) -- down
            keyf (MouseButton LeftButton) =
                mRotate dry (vector3d 1 0 0) . mRotate (-drx) (vector3d 0 1 0)
            keyf (MouseButton RightButton) =
                mRotate drx (vector3d 0 0 1)
            keyf _ = id
    
    keyboard :: a -> Key -> KeyState -> Modifiers -> Position -> IO a
    keyboard sim key keyState modifiers pos = return sim
    
    mouseMove :: a -> Position -> IO a
    mouseMove sim pos = return sim
    
    runSimulation :: a -> IO ()
    runSimulation sim' = do
        (_,argv) <- getArgsAndInitialize
        initWindow sim'
        initDisplay sim'
        sim <- initSimulation sim'
        
        simVar <- atomically $ newTMVar sim
        inputVar <- atomically $ newTMVar $ InputState {
            keySet = Set.empty,
            mousePos = (0,0),
            prevMousePos = (0,0)
        }
        
        camera <- initCamera sim
        cameraVar <- atomically $ newTMVar camera
        
        (reshapeCallback $=) . Just $ \size -> do
            camera <- atomically $ readTMVar cameraVar
            reshape sim camera size
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
        
        -- bind passive motion callback for mouse movement polling
        (passiveMotionCallback $=) . Just $ \pos -> do
            let Position posX posY = pos
            atomically $ inputVar $$~ \i -> i { mousePos = (posX,posY) }
            sim <- atomically $ takeTMVar simVar
            sim' <- mouseMove sim pos
            atomically $ putTMVar simVar sim'
        
        -- more mouse polling (when buttons are down)
        (motionCallback $=) . Just $ \pos -> do
            let Position posX posY = pos
            atomically $ inputVar $$~ \i -> i { mousePos = (posX,posY) }
            sim <- atomically $ takeTMVar simVar
            sim' <- mouseMove sim pos
            atomically $ putTMVar simVar sim'
        
        -- bind keyboard callback
        (keyboardMouseCallback $=) . Just $
            \key keyState modifiers pos -> do
                when (key == Char '\27') leaveMainLoop -- esc
                -- update key set
                atomically $ inputVar $$~ \i -> i {
                    keySet = ($ keySet i) $ case keyState of
                        Down -> Set.insert key
                        Up -> Set.delete key
                }
                -- run user callback
                sim <- atomically $ takeTMVar simVar
                sim' <- keyboard sim key keyState modifiers pos
                atomically $ putTMVar simVar sim'
        
        -- navigation gets its own thread with regular atomic updates
        forkIO $ forever $ runAtFPS 100 $ atomically $ do
            sim <- readTMVar simVar
            input <- readTMVar inputVar
            cameraVar $$~ navigate sim input
            inputVar $$~ \i -> i { prevMousePos = mousePos input }
        
        -- run display callback with helper stuff
        displayCallback $= do
            sim <- atomically $ takeTMVar simVar
            clearColor $= (winBG $ window sim)
            clear [ ColorBuffer, DepthBuffer ]
            
            camera <- atomically $ readTMVar cameraVar
            projection sim camera
            multMatrix $ cameraMatrix camera
            matrixMode $= Modelview 0
            
            loadIdentity
            rotate (-90) $ vector3f 1 0 0 -- z-up
            
            sim' <- postDisplay =<< display =<< preDisplay sim
            atomically $ putTMVar simVar sim'
            
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
