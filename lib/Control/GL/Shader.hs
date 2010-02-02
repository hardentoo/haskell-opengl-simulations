{-# LANGUAGE QuasiQuotes #-} 
module Control.GL.Shader (
    here, newProgram, withProgram, bindProgram
) where
import Graphics.UI.GLUT
import Control.Monad (when,unless)
import Data.Maybe (isJust,fromJust)

import Language.Haskell.TH.Quote 
import Language.Haskell.TH.Syntax 
import Language.Haskell.TH.Lib 

here :: QuasiQuoter -- heredocs
here = QuasiQuoter (litE . stringL) (litP . stringL) 

-- create a program from vertex and fragment shader sources
newProgram :: String -> String -> IO Program
newProgram vertexSrc fragSrc = do
    vShader <- compile vertexSrc
    fShader <- compile fragSrc
    
    [prog] <- genObjectNames 1
    attachedShaders prog $= ([vShader], [fShader])
    linkProgram prog
    reportErrors
    ok <- get $ linkStatus prog
    unless ok $ do
        putStrLn =<< (get $ programInfoLog prog)
        exitApp "Shader failed to compile"
    return prog

-- bind uniform variables to a program object
bindProgram :: Uniform a => Program -> String -> a -> IO ()
bindProgram prog key value = do
    location <- get $ uniformLocation prog key
    reportErrors
    uniform location $= value

-- run a shader over some stateful operations
withProgram :: Program -> IO () -> IO ()
withProgram prog m = do
    currentProgram $= Just prog
    m
    currentProgram $= Nothing

-- compile a shader from source (hidden)
compile :: Shader s => String -> IO s
compile src = do
    [shader] <- genObjectNames 1
    shaderSource shader $= [src]
    compileShader shader
    reportErrors
    ok <- get $ compileStatus shader
    unless ok $ do
        putStrLn =<< (get $ shaderInfoLog shader)
        exitApp "Shader failed to compile"
    return shader

-- exit opengl app (hidden)
exitApp :: String -> IO ()
exitApp msg = do
    leaveMainLoop
    win <- get currentWindow
    when (isJust win) $ destroyWindow (fromJust win)
    fail msg
