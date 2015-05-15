CC=clang++
CXXFLAGS=-std=c++1y -Wall
LDFLAGS=$(shell /usr/local/opt/llvm/bin/llvm-config --cppflags --ldflags --libs core native bitwriter support --system-libs)
TARGET=klc

VALGRIND=valgrind
VALGRINDFLAGS=--leak-check=full --show-leak-kinds=all

all:
	$(CC) $(CXXFLAGS) $(LDFLAGS) -O2 klc.cpp -o $(TARGET)

debug:
	$(CC) $(CXXFLAGS) $(LDFLAGS) -g -UNDEBUG klc.cpp -o $(TARGET)

memcheck:
	$(VALGRIND) $(VALGRINDFLAGS) ./$(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET)
	rm -f *.bc
	rm -f *.o
	rm -f *.ll
	rm -f *.out
	rm -rf *.dSYM
