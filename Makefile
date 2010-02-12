all: hw1

hw1:
	ghc --make -threaded -o hw1 \
		hw/1/Main.hs \
		lib/Graphics/UI/GLUT/Simulation.hs \
		lib/Data/GL.hs \
		lib/Data/GL/Vector.hs \
		lib/Data/GL/Matrix.hs \
		lib/Data/GL/IO.hs \
		lib/Control/GL/Shader.hs \

hw1i:
	ghci \
		hw/1/Main.hs \
		lib/Graphics/UI/GLUT/Simulation.hs \
		lib/Data/GL.hs \
		lib/Data/GL/Vector.hs \
		lib/Data/GL/Matrix.hs \
		lib/Data/GL/IO.hs \
		lib/Control/GL/Shader.hs \

hw2:
	ghc --make -threaded -o hw2 \
		hw/2/Main.hs \
		lib/Graphics/UI/GLUT/Simulation.hs \
		lib/Data/GL.hs \
		lib/Data/GL/Vector.hs \
		lib/Data/GL/Matrix.hs \
		lib/Data/GL/IO.hs \
		lib/Control/GL/Shader.hs \

hw2i:
	ghci \
		hw/2/Main.hs \
		lib/Graphics/UI/GLUT/Simulation.hs \
		lib/Data/GL.hs \
		lib/Data/GL/Vector.hs \
		lib/Data/GL/Matrix.hs \
		lib/Data/GL/IO.hs \
		lib/Control/GL/Shader.hs \

clean:
	rm -f hw1 hw2
	find -name \*.hi -exec rm {} \;
	find -name \*.o -exec rm {} \;
