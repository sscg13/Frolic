EXE := Prolix

SOURCES := Prolix.cpp

CXX := clang++

CXXFLAGS := -O3 -march=native -std=c++2b -static -pthread

SUFFIX := .exe

OUT := $(EXE)$(SUFFIX)


$(EXE): $(SOURCES)
	$(CXX) $^ $(CXXFLAGS) -o $(OUT)