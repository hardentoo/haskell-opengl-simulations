all: hw1

hw1:
	ghc --make -o hw1 \
		hw/1/Main.hs \
		src/Graphics/UI/GL/Simulation.hs \
		src/Data/Matrix/GL.hs

clean:
	rm -f hw1
