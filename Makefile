EXE := concurrent.exe state.exe ref.exe transaction.exe echo.exe delimcc.exe \
	dyn_wind.exe generator.exe promises.exe queens.exe reify_reflect.exe \
	MVar_test.exe chameneos.exe memo.exe nondeterminism.exe nim.exe \
	eratosthenes.exe pipes.exe loop.exe clone_is_tricky.exe fringe.exe \
	algorithmic_differentiation.exe

MVAR_EXE := chameneos_systhr.exe chameneos_lwt.exe chameneos_monad.exe chameneos-ghc.exe chameneos.exe MVar_test.exe

all: $(EXE)
mvar_all : $(MVAR_EXE)

concurrent.exe: sched.mli sched.ml concurrent.ml
	@dune build concurrent.exe

echo.exe: aio/aio.mli aio/aio.ml aio/echo.ml
	dune build echo.exe --root aio
	cp aio/_build/default/echo.exe .

MVar_test.exe: mvar/MVar_test.ml
	dune build MVar_test.exe --root mvar
	cp mvar/_build/default/MVar_test.exe .

chameneos.exe: mvar/chameneos.ml
	dune build chameneos.exe --root mvar
	cp mvar/_build/default/chameneos.exe

chameneos_systhr.exe: mvar/chameneos_systhr.ml
	dune build chameneos_systhr.exe --root mvar

chameneos_lwt.exe: mvar/chameneos_lwt.ml
	dune build chameneos_lwt.exe --root mvar

chameneos_monad.exe: mvar/chameneos_monad.ml
	dune build chameneos_monad.exe --root mvar

chameneos-ghc.exe: mvar/chameneos.hs
	ghc -o mvar/chameneos-ghc.exe -cpp -XBangPatterns -XScopedTypeVariables \
	-XGeneralizedNewtypeDeriving mvar/chameneos.hs

%.exe: %.ml
	dune build $@

clean:
	dune clean
	dune clean --root mvar
	rm -f mvar/*.exe mvar/*.o mvar/*.hi mvar/dune-project
	rm -f *.exe
	dune clean --root aio
	rm -rf aio/dune-project

.PHONY: clean
