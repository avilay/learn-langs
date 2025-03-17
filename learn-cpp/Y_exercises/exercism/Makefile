ODIR=/Users/avilay/Desktop/temp/exercism
CC=clang++ -std=c++17
CFLAGS=-Wall -I.
OBJ=$(ODIR)/Main.o $(ODIR)/Utils.o $(ODIR)/LogLine.o
HEADERS=LogLine.h Utils.h
exercism: $(OBJ)
	$(CC) -o $(ODIR)/$@ $^

$(ODIR)/%.o: %.cpp $(HEADERS) setup
	$(CC) -c $(CFLAGS) -o $@ $<

.PHONY: clean setup

clean:
	rm -fr $(ODIR)

setup:
	mkdir -p $(ODIR)
