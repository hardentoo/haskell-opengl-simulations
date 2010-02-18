module Main where
import Graphics.UI.Simulation3D
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)

data SphereSim = SphereSim

instance Simulation SphereSim where
    navigator = wasd $ WASD { rSpeed = 0.001, tSpeed = 0.05 }
    
    display = liftIO $ runAtFPS 60 $ do
        color3fM 0.8 0.8 1
        drawFloor
        
        color3fM 1 0 0
        renderObject Solid $ Sphere' 1.0 12 8

    onKeyDown (Char ' ') = do
        c <- getCamera
        liftIO $ do
            print $ cameraMatrix c
    onKeyDown _ = return ()
    
drawFloor :: IO ()
drawFloor = renderPrimitive Lines $ do
    forM_ [ -5.0, -4.5 .. 5.0 ] $ \i ->
        mapM_ vertex [
            vertex3f i (-5) 0, vertex3f i 5 0, -- x-lines
            vertex3f (-5) i 0, vertex3f 5 i 0 -- y-lines
        ]

main :: IO () -- what a tiny main!
main = do
    putStrLn "*** Haskell OpenGL Simulator Magic ***\n\
        \    To quit, press escape.\n\
        \    Keys:\n\
        \        forward => w, back => a,\n\
        \        left => s, right => d\n\
        \        up => q, down => z\n\
        \    Rotate: drag left mouse button\n\
        \    Skewer: drag right mouse button\n\
        \ "
    runSimulation SphereSim
