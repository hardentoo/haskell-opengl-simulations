{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, RankNTypes #-}
-- easily extensible GLUT simulation application with reasonable defaults
module Graphics.UI.Simulation3D.Base (
    Simulation(
        begin, display, runSimulationState, navigator, projection,
        onReshape, onMouseMove, onMouseDown, onMouseUp, onKeyDown, onKeyUp
    ),
    SimWindow(..), Camera(..), SimState(..), InputState(..),
    HookIO, KeySet,
    getSimulation, setSimulation,
    getCamera, setCamera,
    getInputState, setInputState,
    getKeySet, setKeySet,
    getMousePos, setMousePos,
    getPrevMousePos, setPrevMousePos,
    getWindow, setWindow,
    getWindowSize, setWindowSize,
    getWindowBG, setWindowBG
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
import Control.Concurrent (ThreadId,forkIO,threadDelay)
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

class Simulation a where
    display :: HookIO a ()
    display = return ()
    
    begin :: HookIO a ()
    begin = return ()
    
    navigator :: HookIO a ()
    navigator = return ()
    
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
    
    onMouseDown :: HookIO a ()
    onMouseDown = return ()
    
    onMouseUp :: HookIO a ()
    onMouseUp = return ()
    
    onKeyDown :: Key -> HookIO a ()
    onKeyDown _ = return ()
    
    onKeyUp :: Key -> HookIO a ()
    onKeyUp _ = return ()
    
    runSimulationState :: SimState a -> IO ()
    runSimulationState state = do
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
    
    getWindowBG :: SimGet a (Color4 GLclampf)
    getWindowBG = simWinBG <$> getWindow
    
    setWindowBG :: SimSet a (Color4 GLclampf)
    setWindowBG size = setWindow . f =<< getWindow
        where f s = s { simWinBG = size }
    
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
        mapM ($ stateVar) [
                bindReshape,
                bindPassiveMotion,
                bindActiveMotion,
                bindKeyboardMouse,
                bindDisplay
            ]
        startNavigation stateVar
        actionOnWindowClose $= MainLoopReturns -- ghci stays running
    
    -- callback to set projection matrix and update window state
    bindReshape :: MVar (SimState a) -> IO ()
    bindReshape stateVar = (reshapeCallback $=) . Just $ \size@(Size w h) -> do
        viewport $= (Position 0 0, size)
        matrixMode $= Modelview 0
        let cb = setWindowSize (w,h) >> projection >> onReshape
        putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
    
    -- bind passive motion callback for mouse movement polling
    bindPassiveMotion :: MVar (SimState a) -> IO ()
    bindPassiveMotion stateVar = (passiveMotionCallback $=) . Just $ \pos -> do
        let Position posX posY = pos
            cb = setMousePos (posX,posY) >> onMouseMove
        putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
     
    -- bind active motion callback for mouse polling when buttons are down
    bindActiveMotion :: MVar (SimState a) -> IO ()
    bindActiveMotion stateVar = (motionCallback $=) . Just $ \pos -> do
        let Position posX posY = pos
            cb = setMousePos (posX,posY) >> onMouseMove
        putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
    
    -- binding for keyboard and mouse button events
    bindKeyboardMouse :: MVar (SimState a) -> IO ()
    bindKeyboardMouse stateVar = (keyboardMouseCallback $=) . Just $
        \key keyState modifiers pos -> do
            when (key == Char '\27') leaveMainLoop -- esc
            -- update key set
            let cb = (setKeySet . f =<< getKeySet) >> ev
                f = case keyState of
                    Down -> Set.insert key
                    Up -> Set.delete key
                ev = case (key,keyState) of
                    (MouseButton _,Down) -> onMouseDown
                    (MouseButton _,Up) -> onMouseUp
                    (_,Down) -> onKeyDown key
                    (_,Up) -> onKeyUp key
            -- run user callback along with key set housekeeping
            putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
    
    -- run display callback with helper stuff
    bindDisplay :: MVar (SimState a) -> IO ()
    bindDisplay stateVar = (displayCallback $=) $ do
        let
            cb :: Simulation a => ST.StateT (SimState a) IO ()
            cb = do
                bg <- getWindowBG
                liftIO $ do
                    clearColor $= bg
                    clear [ ColorBuffer, DepthBuffer ]
                
                projection
                cam <- getCamera
                liftIO $ do
                    (multMatrix =<<) . toGLmat $ cameraMatrix cam
                    matrixMode $= Modelview 0
                    loadIdentity
                
                display
                liftIO $ do
                    flush
                    swapBuffers
                    postRedisplay Nothing
         
        putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
        return ()
    
    startNavigation :: MVar (SimState a) -> IO ThreadId
    startNavigation stateVar = forkIO $ forever $ runAtFPS 50 $ do
        let cb = navigator >> (setPrevMousePos =<< getMousePos)
        putMVar stateVar =<< ST.execStateT cb =<< takeMVar stateVar
