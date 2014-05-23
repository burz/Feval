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

examples: $(SRCS)
	ghc examples.hs

feval: $(SRCS)
	ghc feval.hs

clean:
	rm -f *.o *.hi && rm -f FVL/*.o FVL/*.hi

