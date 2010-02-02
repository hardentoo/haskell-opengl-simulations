{-# LANGUAGE QuasiQuotes #-} 
module Control.GL.Shader (
    here, newProg
) where
import Graphics.UI.GLUT
import Control.Monad

import Language.Haskell.TH.Quote 
import Language.Haskell.TH.Syntax 
import Language.Haskell.TH.Lib 

here :: QuasiQuoter -- heredocs
here = QuasiQuoter (litE . stringL) (litP . stringL) 

-- create a program from vertex and fragment shader sources
newProg :: String -> String -> IO Program
newProg vertexSrc fragSrc = do
    vShader <- compile vertexSrc
    fShader <- compile fragSrc
    
    [prog] <- genObjectNames 1
    attachedShaders prog $= ([vShader], [fShader])
    linkProgram prog
    reportErrors
    ok <- get $ linkStatus prog
    unless ok $ do
        infoLog <- get $ programInfoLog prog
        fail $ show infoLog
    return prog

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
        infoLog <- get $ shaderInfoLog shader
        fail $ show infoLog
    return shader
