all: hw1

hw1:
	ghc --make -o hw1 \
		hw/1/Main.hs \
		lib/Graphics/UI/GL/Simulation.hs \
		lib/Data/Matrix/GL.hs

clean:
	rm -f hw1
