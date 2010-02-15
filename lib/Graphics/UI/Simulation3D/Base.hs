{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, RankNTypes #-}
-- easily extensible GLUT simulation application with reasonable defaults
module Graphics.UI.Simulation3D.Base (
    Simulation(begin,display,runSimulation),
    SimWindow(..), Camera(..), SimState(..), InputState(..),
    HookIO, NavigateHook, KeySet,
    getCamera, getInputState, getKeySet, getMousePos, getPrevMousePos
) where

import Graphics.UI.GLUT hiding (Matrix(..),newMatrix,rotate,translate)
import Graphics.UI.Simulation3D.Util

import Control.Monad (when,forever)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as ST
import Control.Concurrent.MVar

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

type SimGet a b = (ST.MonadState (SimState a) m, Functor m) => m b
type SimSet a b = (ST.MonadState (SimState a) m, Functor m) => b -> m ()

type HookIO a b = ST.StateT (SimState a) IO b
type NavigateHook a = ST.State (SimState a) Camera

class Simulation a where
    display :: HookIO a ()
    display = return ()
    
    begin :: HookIO a ()
    begin = return ()
    
    projection :: HookIO a ()
    projection = do
        (w,h) <- getWindowSize
        cam <- getCamera
        let
            fov = cameraFOV cam
            near = cameraNear cam
            far = cameraFar cam
            aspect = fromIntegral w / fromIntegral h
        liftIO $ do
            matrixMode $= Projection
            loadIdentity
            perspective fov aspect near far
     
    onReshape :: HookIO a ()
    onReshape = return ()
    
    onMouseMove :: HookIO a ()
    onMouseMove = return ()
    
    runSimulation :: SimState a -> IO ()
    runSimulation state = do
        initWindow state -- initialize the window
        initDisplay state -- and the display
        state' <- ST.execStateT begin state -- run the user's begin hook
        bindCallbacks state'
        mainLoop
    
    -- Getters and setters for convenience.
    -- Sometime later these will have a bit nicer interface.
    
    getSimulation :: SimGet a a
    getSimulation = simulation <$> ST.get
    
    setSimulation :: SimSet a a
    setSimulation sim = ST.modify $ \s -> s { simulation = sim }
    
    getCamera :: SimGet a Camera
    getCamera = simCamera <$> ST.get
    
    setCamera :: SimSet a Camera
    setCamera cam = ST.modify $ \s -> s { simCamera = cam }
    
    getInputState :: SimGet a InputState
    getInputState = simInputState <$> ST.get
    
    setInputState :: SimSet a InputState
    setInputState is = ST.modify $ \s -> s { simInputState = is }
    
    getKeySet :: SimGet a KeySet
    getKeySet = inputKeySet <$> getInputState
    
    setKeySet :: SimSet a KeySet
    setKeySet set = setInputState . f =<< getInputState
        where f s = s { inputKeySet = set }
    
    getMousePos :: SimGet a (GLint,GLint)
    getMousePos = inputMousePos <$> getInputState
    
    setMousePos :: SimSet a (GLint,GLint)
    setMousePos pos = setInputState . f =<< getInputState
        where f s = s { inputMousePos = pos }
    
    getPrevMousePos :: SimGet a (GLint,GLint)
    getPrevMousePos = inputPrevMousePos <$> getInputState
    
    setPrevMousePos :: SimSet a (GLint,GLint)
    setPrevMousePos pos = setInputState . f =<< getInputState
        where f s = s { inputPrevMousePos = pos }
    
    getWindow :: SimGet a SimWindow
    getWindow = simWindow <$> ST.get
    
    setWindow :: SimSet a SimWindow
    setWindow win = ST.modify $ \s -> s { simWindow = win }
    
    getWindowSize :: SimGet a (GLsizei,GLsizei)
    getWindowSize = simWinSize <$> getWindow
    
    setWindowSize :: SimSet a (GLsizei,GLsizei)
    setWindowSize size = setWindow . f =<< getWindow
        where f s = s { simWinSize = size }
    
    -- Exported functions end here. Helpers below.
    
    initWindow :: SimState a -> IO ()
    initWindow state = do
        -- initialize the window
        (_,argv) <- getArgsAndInitialize
        let win = simWindow state
        initialDisplayMode $= simModes state
        initialWindowSize $= (uncurry Size $ simWinSize win)
        initialWindowPosition $= (uncurry Position $ simWinPos win)
        createWindow $ simWinTitle win
        return ()
    
    initDisplay :: SimState a -> IO ()        
    initDisplay state = do
        -- initialize the display with some reasonable modes
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        shadeModel $= Flat
        depthMask $= Enabled
        depthFunc $= Just Lequal
        lighting $= Disabled
        texture Texture2D $= Enabled
    
    bindCallbacks :: SimState a -> IO ()
    bindCallbacks state = do
        stateVar <- newMVar state
        bindReshape stateVar
        
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
    
    -- callback to set projection matrix and update window state
    bindReshape :: MVar (SimState a) -> IO ()
    bindReshape stateVar = do
        (reshapeCallback $=) . Just $ \size@(Size w h) -> do
            -- get state and update window size
            let uSize s = s { simWindow = (simWindow s) { simWinSize = (w,h) } }
            state <- uSize <$> takeMVar stateVar
            -- update viewport and set new projection
            viewport $= (Position 0 0, size)
            
            ST.execStateT projection state
            
            matrixMode $= Modelview 0
            -- call user's reshape callback
            putMVar stateVar =<< ST.execStateT onReshape state
    
    -- bind passive motion callback for mouse movement polling
    bindPassiveMotion :: MVar (SimState a) -> IO ()
    bindPassiveMotion stateVar = do
        (passiveMotionCallback $=) . Just $ \pos -> do
            let Position posX posY = pos
                cb = setMousePos (posX,posY) >> onMouseMove
            state <- takeMVar stateVar
            putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
     
    -- bind active motion callback for mouse polling when buttons are down
    bindActiveMotion :: MVar (SimState a) -> IO ()
    bindActiveMotion stateVar = do
        (motionCallback $=) . Just $ \pos -> do
            let Position posX posY = pos
                cb = setMousePos (posX,posY) >> onMouseMove
            putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
        
{-
        -- 
        return ()
        
        
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
    
    keyboard :: a -> Key -> KeyState -> Modifiers -> Position -> IO a
    keyboard sim key keyState modifiers pos = return sim
    
    mouseMove :: a -> Position -> IO a
    mouseMove sim pos = return sim
