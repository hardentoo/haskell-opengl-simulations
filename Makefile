all: hw1

hw1:
	ghc --make -threaded -o hw1 \
		hw/1/Main.hs \
		lib/Graphics/UI/Simulation3D/Simulation.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw1i:
	ghci \
		hw/1/Main.hs \
		lib/Graphics/UI/Simulation3D/Simulation.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw2:
	ghc --make -threaded -o hw2 \
		hw/2/Main.hs \
		lib/Graphics/UI/Simulation3D/Simulation.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw2i:
	ghci \
		hw/2/Main.hs \
		lib/Graphics/UI/Simulation3D/Simulation.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

clean:
	rm -f hw1 hw2
	find -name \*.hi -exec rm {} \;
	find -name \*.o -exec rm {} \;
