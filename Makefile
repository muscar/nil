CC=clang++
CXXFLAGS=-std=c++1y -Wall -g
LDFLAGS=$(shell llvm-config-3.4 --cppflags --ldflags --libs core native bitwriter support)
TARGET=klc

VALGRIND=valgrind
VALGRINDFLAGS=--leak-check=full --show-leak-kinds=all

all:
	$(CC) $(CXXFLAGS) $(LDFLAGS) klc.cpp -o $(TARGET)

memcheck:
	$(VALGRIND) $(VALGRINDFLAGS) ./$(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET)
	rm -f *.bc
	rm -f *.o
	rm -f *.ll
	rm -f *.out
