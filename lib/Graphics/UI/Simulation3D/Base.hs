{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-- easily extensible GLUT simulation application with reasonable defaults
module Graphics.UI.Simulation3D.Base (
    Simulation(begin,display,runSimulation),
    SimWindow(..), Camera(..), SimState(..), InputState(..),
    HookIO, HookT, Hook, NavigateHook,
    KeySet, getCamera, getInputState, getKeySet, getMousePos, getPrevMousePos
) where

import Graphics.UI.GLUT hiding (Matrix(..),newMatrix,rotate,translate)
import Graphics.UI.Simulation3D.Util

import Control.Monad (when,forever)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as ST

import Control.Arrow (first,(***))
import Control.Applicative ((<$>))
import Data.Maybe (isJust,fromJust)

import qualified Data.Set as Set
import Control.Concurrent (forkIO,threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Numeric.LinearAlgebra.Transform
import Numeric.LinearAlgebra hiding (scale,reshape)

data SimWindow = SimWindow {
    simWinTitle :: String,
    simWinSize :: (GLsizei,GLsizei),
    simWinPos :: (GLint,GLint),
    simWinBG :: Color4 GLclampf
} deriving (Eq,Show)

data Camera = Camera {
    cameraFOV :: GLdouble,
    cameraNear :: GLdouble,
    cameraFar :: GLdouble,
    cameraMatrix :: Matrix Double
} deriving (Eq,Show)

data Simulation a => SimState a = SimState {
    simulation :: a,
    simCamera :: Camera,
    simWindow :: SimWindow,
    simNavigator :: NavigateHook a,
    simModes :: [DisplayMode],
    simInputState :: InputState,
    simFPS :: Double
}

data InputState = InputState {
    inputKeySet :: KeySet,
    inputMousePos :: (GLint,GLint),
    inputPrevMousePos :: (GLint,GLint)
}

type KeySet = Set.Set Key

getSimulation :: Simulation a => ST.State (SimState a) a
getSimulation = simulation <$> ST.get

getCamera :: Simulation a => ST.State (SimState a) Camera
getCamera = simCamera <$> ST.get

getInputState :: Simulation a => ST.State (SimState a) InputState
getInputState = simInputState <$> ST.get

getKeySet :: Simulation a => ST.State (SimState a) KeySet
getKeySet = inputKeySet <$> getInputState

getMousePos :: Simulation a => ST.State (SimState a) (GLint,GLint)
getMousePos = inputMousePos <$> getInputState

getPrevMousePos :: Simulation a => ST.State (SimState a) (GLint,GLint)
getPrevMousePos = inputPrevMousePos <$> getInputState

type HookIO a b = ST.StateT (SimState a) IO b
type HookT a m = ST.StateT (SimState a) m a
type Hook a = ST.State (SimState a) a
type NavigateHook a = ST.State (SimState a) Camera

class Simulation a where
    display :: HookIO a ()
    display = return ()
    
    begin :: HookIO a a
    begin = simulation <$> ST.get
    
    runSimulation :: SimState a -> IO ()
    runSimulation state = do
        runInitials state
    
    -- End of exported members. It's all helpers from here down
    
    
    runInitials :: SimState a -> IO ()
    runInitials state = do
        (_,argv) <- getArgsAndInitialize
        initialDisplayMode $= simModes state
        let win = simWindow state
        initialWindowSize $= (uncurry Size $ simWinSize win)
        initialWindowPosition $= (uncurry Position $ simWinPos win)
        createWindow $ simWinTitle win
        return ()

{-
        -- 
        return ()
        initDisplay sim'
        sim <- initSimulation sim'
        
        simVar <- atomically $ newTMVar sim
        inputVar <- atomically $ newTMVar $ InputState {
            keySet = Set.empty,
            mousePos = (0,0),
            prevMousePos = (0,0)
        }
        
        cameraVar <- atomically $ newTMVar (initCamera sim)
        
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
        forkIO $ forever $ runAtFPS 50 $ atomically $ do
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
            Size w h <- get windowSize
            projection sim camera (w,h)
            
            multMatrix =<< toGLmat (cameraMatrix camera)
            matrixMode $= Modelview 0
            
            loadIdentity
            rotateM (-90) $ vector3f 1 0 0 -- z-up
            
            sim' <- display sim
            atomically $ putTMVar simVar sim'
            
            flush
            swapBuffers
            postRedisplay Nothing
        
        mainLoop
    -}
    
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
    
    projection :: Integral b => a -> Camera -> (b,b) -> IO ()
    projection sim cam (w,h) = do
        matrixMode $= Projection
        loadIdentity
        let
            fov = cameraFOV cam
            near = cameraNear cam
            far = cameraFar cam
            aspect = fromIntegral w / fromIntegral h
        perspective fov aspect near far
    
    reshape :: a -> Camera -> ReshapeCallback
    reshape sim cam size@(Size w h) = do
        viewport $= (Position 0 0, size)
        projection sim cam (w,h)
        matrixMode $= Modelview 0
    
    keyboard :: a -> Key -> KeyState -> Modifiers -> Position -> IO a
    keyboard sim key keyState modifiers pos = return sim
    
    mouseMove :: a -> Position -> IO a
    mouseMove sim pos = return sim
