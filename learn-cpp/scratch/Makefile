ODIR=/Users/avilay/Desktop/temp/scratch
CC=clang++ -std=c++20
CFLAGS=-Wall -I.
DBGFLAGS=-fcolor-diagnostics -fansi-escape-codes -g
# HEADERS=Casting.h Cookie.h Counter.h Utils.h
# OBJ=$(ODIR)/Scratch.o $(ODIR)/Utils.o $(ODIR)/Cookie.o $(ODIR)/Counter.o $(ODIR)/Casting.o
HEADERS=Utils.h
OBJ=$(ODIR)/Scratch.o $(ODIR)/Utils.o

scratch: $(OBJ)
	$(CC) $(DBGFLAGS) -o $(ODIR)/$@ $^

$(ODIR)/%.o: %.cpp $(HEADERS) setup
	$(CC) $(DBGFLAGS) -c $(CFLAGS) -o $@ $<

.PHONY: clean setup run dbgflgs

clean:
	rm -fr $(ODIR)

setup:
	mkdir -p $(ODIR)	

run: scratch
	echo "" && $(ODIR)/$<	