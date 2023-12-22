default: faster

run:
	runhaskell main.hs

faster:
	ghc -O3 main.hs
	rm main.o
	rm main.hi
	./main
	rm main
