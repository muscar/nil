CC=clang++
CXXFLAGS=-std=c++1y -g
LDFLAGS=$(shell llvm-config-3.4 --cppflags --ldflags --libs core jit native)
TARGET=klc

all:
	$(CC) $(CXXFLAGS) $(LDFLAGS) klc.cpp -o $(TARGET)

clean:
	rm -f $(TARGET)
