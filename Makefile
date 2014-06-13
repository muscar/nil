CC=clang++
CXXFLAGS=-std=c++1y -Wall -g
LDFLAGS=$(shell llvm-config-3.4 --cppflags --ldflags --libs core native bitwriter support)
TARGET=klc

all:
	$(CC) $(CXXFLAGS) $(LDFLAGS) klc.cpp -o $(TARGET)

exe:
	llc-3.4 -filetype=obj out.bc -o out.o
	clang -std=c11 -O0 -Wall -Werror -c lib.c
	clang lib.o out.o -o out


clean:
	rm -f $(TARGET)
	rm *.bc
	rm *.o
	rm *.ll
	rm out
