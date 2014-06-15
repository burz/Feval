SRCS = FVL/Algebra.hs \
       FVL/EF.hs \
       FVL/EFAST.hs \
       FVL/Eval.hs \
       FVL/EvalAST.hs \
       FVL/F.hs \
       FVL/FAST.hs \
       FVL/Lexer.hs \
       FVL/Parser.hs \
       FVL/Type.hs \
       FVL/TypeAST.hs

all: examples feval

examples: $(SRCS) examples.hs
	ghc -rtsopts examples.hs

feval: $(SRCS) feval.hs
	ghc feval.hs

clean:
	rm -f *.o *.hi FVL/*.o FVL/*.hi

