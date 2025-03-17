# Here I have introduced three new targets - build, setup, and clean. By themselves it is pretty
# clear what the make utility will do if I give `make clean` or `make setup` commands. However
# when these targets appear as depedencies of other targets, like in the build target or the
# $(ODIR)/%.o target, there is a potential for confusion if we also have files with these names.
# Now the make utilty does not know whether the term refers to another target or the file. To
# disambiguate this we use yet another special thing - .PHONY - which tells the make utility
# that the dependecies of this special target are all targets. Conceptually I think of these
# targets as tasks.

CC=clang++
CFLAGS=-I.
DEPS=utils/Point.h utils/Triangle.h
ODIR=obj
OBJ=$(ODIR)/utils/Point.o $(ODIR)/utils/Triangle.o $(ODIR)/app/main.o
LIBS=-lglfw.3 -lGLEW.2.1
FWORKS=-framework OpenGL -framework Cocoa -framework IOKit

$(ODIR)/%.o: %.cpp $(DEPS) setup
	$(CC) -c $(CFLAGS) -o $@ $<

opengldemo: $(OBJ)
	$(CC) -o bin/$@ $(LIBS) $(FWORKS) $^

.PHONY: clean setup

clean:
	rm -f $(ODIR)/utils/*.o $(ODIR)/app/*.o bin/opengldemo

setup:
	mkdir -p $(ODIR)/utils $(ODIR)/app bin

build: clean opengldemo
