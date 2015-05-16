CC=clang++-3.7
CXXFLAGS=-std=c++1y -Wall
CXXFLAGS+=$(shell llvm-config-3.7 --cppflags)
LDFLAGS=$(shell llvm-config-3.7 --ldflags --libs core native bitwriter support --system-libs)
TARGET=klc

VALGRIND=valgrind
VALGRINDFLAGS=--leak-check=full --show-leak-kinds=all

all:
	$(CC) $(CXXFLAGS) -O2 klc.cpp -o $(TARGET) $(LDFLAGS)

debug:
	$(CC) $(CXXFLAGS) -g -UNDEBUG klc.cpp -o $(TARGET) $(LDFLAGS)

memcheck:
	$(VALGRIND) $(VALGRINDFLAGS) ./$(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET)
	rm -f *.bc
	rm -f *.o
	rm -f *.ll
	rm -f *.out
	rm -rf *.dSYM
