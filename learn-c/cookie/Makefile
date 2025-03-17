app: main scratch intarray
	clang -g -Wall -o bin/app bin/main.o bin/scratch.o bin/IntArray.o

intarray: IntArray/IntArray.h IntArray/IntArray.c
	clang -Wall -g -o bin/IntArray.o -c IntArray/IntArray.c

scratch: scratch.h scratch.c
	clang -Wall -g -o bin/scratch.o -c scratch.c

main: main.c
	clang -Wall -g -o bin/main.o -c main.c

clean:
	rm -fr ./bin
	mkdir ./bin