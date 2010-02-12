-- raytrace several ellipsoids
-- James Halliday

{-# LANGUAGE QuasiQuotes #-} -- for magical heredocs
module Main where
import Graphics.UI.GLUT.Simulation
import Control.GL.Shader (newProgram,withProgram,bindProgram,here)
import Control.Monad (forM_,liftM2)
import Data.Bits (xor)
import Foreign (newArray)
import Data.String.Utils (replace)

-- With the simulator typeclass, just create your own datatypes...
data EllipsoidSim = EllipsoidSim {
    simShader :: Program,
    checkerTex :: PixelData (Color4 GLfloat)
}

-- ...and then create instances with your type defining callbacks and such 
instance Simulation EllipsoidSim where
    display sim = runAtFPS 60 $ do
        let prog = simShader sim
        color3fM 0.8 0.8 1
        drawFloor sim
        
        -- It turns out that the camera position can be taken right out of the
        -- projection matrix, so no need to bind any uniform variables.
        withProgram prog $ preservingMatrix $ do
            color3fM 0 1 1
            renderObject Solid $ Sphere' 10 6 6
        return sim
    
    initSimulation sim = do
        prog <- newProgram vertexShader fragmentShader
        return $ sim { simShader = prog }

vertexShader :: String
vertexShader = [$here|
    // -- vertex shader
    varying vec3 offset; // -- normalized vector offset of camera
    varying vec3 camera; // -- camera in world coords
    
    void main() {
        // -- Translation is the third column of the projection inverse.
        camera = vec3(gl_ProjectionMatrixInverse[3]);
        
        vec3 mv = vec3(gl_ModelViewMatrix * gl_Vertex);
        offset = normalize(camera - mv);
        
        gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
    }
|]

shaderPrelude :: String
shaderPrelude = [$here|
    const mat4 identity = mat4(
        vec4(1.0, 0.0, 0.0, 0.0),
        vec4(0.0, 1.0, 0.0, 0.0),
        vec4(0.0, 0.0, 1.0, 0.0),
        vec4(0.0, 0.0, 0.0, 1.0)
    );
    
    mat4 rotate(float angle, vec3 axis) {
        // http://en.wikipedia.org/wiki/Rotation_matrix#Axis_and_angle
        float c = cos(angle), s = sin(angle);
        vec3 s3 = axis * s;
        vec3 c3 = axis * vec3(1.0 - c);
        vec3 sw = axis * c3.yzx;
        return mat4(
            vec4(
                axis.x * c3.x + c,
                sw.x - s3.z,
                c3.z + s3.y,
                0.0
            ),
            vec4(
                sw.x + s3.z,
                axis.y * c3.y + c,
                sw.y - s3.x,
                0.0
            ),
            vec4(
                sw.z - s3.y,
                sw.y + s3.x,
                axis.z * c3.z + c,
                0.0
            ),
            vec4(0.0, 0.0, 0.0, 1.0)
        );
        
        /*
        c = cos(θ); s = sin(θ); C = 1-c
        xs = x*s;   ys = y*s;   zs = z*s
        xC = x*C;   yC = y*C;   zC = z*C
        xyC = x*yC; yzC = y*zC; zxC = z*xC
        [ x*xC+c   xyC-zs   zxC+ys ]
        [ xyC+zs   y*yC+c   yzC-xs ]
        [ zxC-ys   yzC+xs   z*zC+c ]
        */
    }
|]

fragmentShader :: String
fragmentShader = shaderPrelude ++ [$here|
    // -- fragment shader
    varying vec3 camera;
    varying vec3 offset;
    
    // Compute the intersection of an ellipse with the camera ray.
    // Returns the a vec4 with the point of intersection and solution t in the
    // vec4.w slot.
    vec4 ellipse(vec4 eq, mat4 emat) {
        // -- Solve for the intersection of the ray with an ellipse
        // -- described by ax² + by² + cz² = -k
        float a = eq.x, b = eq.y, c = eq.z, k = -eq.w;
        // P(t) = C + t * D, t >= 0
        
        vec3 C = vec3(emat * vec4(camera, 1.0));
        vec3 D = offset;
        
        // a_, b_, and c_ used to compute quadratic equation
        float a_ = (a * D.x * D.x) + (b * D.y * D.y) + (c * D.z * D.z);
        float b_ = 2.0 * (
            (a * C.x * D.x) + (b * C.y * D.y) + (c * C.z * D.z)
        );
        float c_ = (a * C.x * C.x) + (b * C.y * C.y) + (c * C.z * C.z) + k;
        
        vec3 point = vec3(0.0);
        float t = -1.0; // default to non-real answer
        if (b_ * b_ >= 4.0 * a_ * c_) { // real answer
            float t1 = (-b_ + sqrt(b_ * b_ - 4.0 * a_ * c_)) / (2.0 * a_);
            float t2 = (-b_ - sqrt(b_ * b_ - 4.0 * a_ * c_)) / (2.0 * a_);
            
            if (t1 >= 0.0 && t2 >= 0.0) {
                t = min(t1,t2); // two solutions, pick nearest
            }
            else {
                t = max(t1,t2); // one solution, pick >= 0
            }
            point = C + t * D; // point of intersection
        }
        return vec4(point,t);
    }
    
    // Compute the normalized gradient of the ellipse from the equation and the
    // point of intersection.
    vec3 gradient(vec4 eq, vec3 p) {
        return normalize(vec3(
            2.0 * eq.x * p.x,
            2.0 * eq.y * p.y,
            -2.0 * eq.z * p.z
        ));
    }
    
    void main() {
        vec4 eq1 = vec4(1.0, 0.7, 2.0, 1.0);
        vec4 e1 = ellipse(
            eq1,
            rotate(30.0, vec3(1,0,0))
            /*
            mat4(
                vec4(1.0, 0.0, 0.0, 0.0),
                vec4(0.0, 1.0, 0.0, 0.0),
                vec4(0.0, 0.0, 1.0, 0.0),
                vec4(0.0, 0.0, 0.0, 1.0)
            )
            */
        );
        
        vec4 eq2 = vec4(4.0, 0.3, 0.5, 2.0);
        float a2 = 30.0;
        vec4 e2 = ellipse(
            eq2,
            mat4( // the worst way to possibly do this
                // you could rotate these...
                vec4(1.0, 0.0, 0.0, 0.0),
                vec4(0.0, 1.0, 0.0, 0.0),
                vec4(0.0, 0.0, 1.0, 0.0),
                vec4(3.0, -2.0, 0.0, 1.0)
            )
        );
        
        //
        if (e1.w < 0.0 && e2.w < 0.0) discard;
        
        // figure out which ellipse is intersecting
        float which = 2.0;
        if (e2.w < 0.0 || e1.w < e2.w) which = 1.0;
        
        vec3 ei; // ellipse intersection
        vec4 eq; // matching equation
        
        if (which == 1.0) {
            ei = vec3(e1);
            eq = eq1;
        }
        else {
            ei = vec3(e2);
            eq = eq2;
        }
        
        vec3 g = gradient(eq,ei);
        
        // update depth buffer accordingly
        vec4 proj = gl_ProjectionMatrix * vec4(vec3(ei),1.0);
        gl_FragDepth = 0.5 + 0.5 * (proj.z / proj.w);
        
        if (which == 1.0) {
            gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
        }
        else {
            gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0);
        }
    }
|]
        
-- Draw a floor, obviously!
drawFloor :: EllipsoidSim -> IO ()
drawFloor sim = do
    tex : [] <- genObjectNames 1
    textureBinding Texture2D $= Just tex
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    
    texImage2D
        Nothing -- no cube maps
        NoProxy -- standard texture 2d
        0 -- level 0
        RGBA' -- internal format
        (TextureSize2D 19 19) -- texture size
        0 -- border
        (checkerTex sim) -- pointer to the blurred texture
    
    color4fM 1 1 1 1
    texture Texture2D $= Enabled
    renderPrimitive Quads $ do
        texCoord2fM 0 0 >> vertex3fM (-5) (-5) 0
        texCoord2fM 0 1 >> vertex3fM 5 (-5) 0
        texCoord2fM 1 1 >> vertex3fM 5 5 0
        texCoord2fM 1 0 >> vertex3fM (-5) 5 0
    texture Texture2D $= Disabled
    deleteObjectNames [tex]

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
    
    ptr <- newArray $ take (19 * 19)
        $ cycle [ color4f 0 0 0 0.5, color4f 1 1 1 0.5 ]
    
    runSimulation $ EllipsoidSim {
        simShader = undefined, -- filled in by initSimulation
        checkerTex = PixelData RGBA Float ptr
    }
