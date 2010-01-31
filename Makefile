all: hw1

hw1:
	ghc --make -o hw1 \
		hw/1/Main.hs lib/Simulation.hs

clean:
	rm -f hw1
