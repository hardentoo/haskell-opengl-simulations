{-# LANGUAGE QuasiQuotes #-} 
module Control.GL.Shader (
    here, newProgram, withProgram
) where
import Graphics.UI.GLUT
import Control.Monad

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
    return prog

withProgram :: Program -> IO () -> IO ()
withProgram prog m = do
    currentProgram $= Just prog
    m
    currentProgram $= Nothing

compile :: Shader s => String -> IO s
compile src = do
    putStrLn "compile begin"
    [shader] <- genObjectNames 1
    putStrLn "generated"
    shaderSource shader $= [src]
    compileShader shader
    reportErrors
    ok <- get $ compileStatus shader
    unless ok $ do
        putStrLn =<< (get $ shaderInfoLog shader)
    return shader
