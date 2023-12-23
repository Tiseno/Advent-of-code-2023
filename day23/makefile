default:
	runhaskell main.hs

faster:
	ghc -O3 main.hs
	rm main.o
	rm main.hi
	./main
	rm main

graph:
	dot -Tsvg graph.dot -o graph.svg ; open graph.svg

exampleGraph:
	dot -Tsvg example.graph.dot -o example.graph.svg ; open example.graph.svg
