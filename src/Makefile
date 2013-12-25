all: Main

Main: Main.hs Latex.hs Rep.hs
	ghc Main.hs Latex.hs Rep.hs -o CrosswordHelper -O2 -Wall

clean:
	rm *.o *.hi