CC=clang++
CXXFLAGS=-std=c++1y -Wall -O3
LDFLAGS=$(shell llvm-config-3.4 --cppflags --ldflags --libs core jit native bitwriter support)
TARGET=klc

all:
	$(CC) $(CXXFLAGS) $(LDFLAGS) klc.cpp -o $(TARGET)

exe:
	llc-3.4 -filetype=obj out.bc -o out.o
	clang -std=c11 -O3 -Wall -Werror -c lib.c
	clang lib.o out.o -o out


clean:
	rm -f $(TARGET)
	rm *.bc
	rm *.o
	rm out
