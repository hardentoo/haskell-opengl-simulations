all: sphere hw1 hw2

sphere:
	ghc --make -threaded -o sphere -static \
		examples/Sphere.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

spherei:
	ghci \
		examples/Sphere.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw1:
	ghc --make -threaded -o hw1 \
		hw/1/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw1i:
	ghci \
		hw/1/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw2:
	ghc --make -threaded -o hw2 \
		hw/2/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw2i:
	ghci \
		hw/2/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw3:
	ghc --make -threaded -o hw3 \
		hw/3/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

hw3i:
	ghci \
		hw/3/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

proj1:
	ghc --make -threaded -o hw3 \
		hw/proj1/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

proj1i:
	ghci \
		hw/proj1/Main.hs \
		lib/Graphics/UI/Simulation3D.hs \
		lib/Graphics/UI/Simulation3D/Base.hs \
		lib/Graphics/UI/Simulation3D/Util.hs \
		lib/Graphics/UI/Simulation3D/Navigator.hs \
		lib/Graphics/UI/Simulation3D/Shader.hs \

clean:
	rm -f hw1 hw2
	find -name \*.hi -exec rm {} \;
	find -name \*.o -exec rm {} \;
