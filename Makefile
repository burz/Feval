all: examples feval

examples:
		ghc examples.hs

feval:
		ghc feval.hs

clean:
		rm -f *.o *.hi examples feval && rm -f FVL/*.o FVL/*.hi

