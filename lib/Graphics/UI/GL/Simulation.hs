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
    
    reshape :: a -> IORef Camera -> ReshapeCallback
    reshape sim cameraRef size@(Size w h) = do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        cam <- get cameraRef
        let
            fov = cameraFOV cam
            near = cameraNear cam
            far = cameraFar cam
            w' = fromIntegral w
            h' = fromIntegral h
        perspective fov (w' / h') near far
        matrixMode $= Modelview 0
    
    navigation :: a -> KeySet -> GLmatrix GLdouble -> IO (GLmatrix GLdouble)
    navigation sim keys mat = do
        print keys
        return $ foldl (\m k -> keyf k m) mat $ Set.elems keys
            where
                keyf (Char 'w') = mTranslate (vector3d 0 1 0)
                keyf _ = id
    
    keyboard :: a -> KeyboardMouseCallback
    keyboard sim key keyState modifiers pos = return ()
    
    runSimulation :: a -> IO ()
    runSimulation sim = do
        (_, argv) <- getArgsAndInitialize
        initWindow sim
        initDisplay sim
        
        simRef <- newIORef sim
        cameraRef <- newIORef =<< initCamera sim
        keySetRef <- newIORef Set.empty
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
        (keyboardMouseCallback $=) . Just $
            \key keyState modifiers pos -> do
                when (key == Char '\27') leaveMainLoop -- esc
                -- update key set
                (keySetRef $~) $ case keyState of
                    Down -> Set.insert key
                    Up -> Set.delete key
                -- run user callback
                keyboard sim key keyState modifiers pos
        
        reshapeCallback $= Just (reshape sim cameraRef)
        
        -- navigation gets its own thread with regular updates
        forkIO $ forever $ do
            t' <- elapsed $ do
                sim <- get simRef
                cam <- get cameraRef
                keys <- get keySetRef
                mat <- navigation sim keys (cameraMatrix cam)
                cameraRef $= cam { cameraMatrix = mat }
            let t = max 0.0 (0.04 - t')
            -- ~(1/25) seconds between updates
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
