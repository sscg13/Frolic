EXE := Frolic
EVALFILE := chess-net15.nnue
ARCH := native

SOURCES := Frolic.cpp

CXX := clang++

CXXFLAGS := -O3 -march=$(ARCH) -static -pthread -DEUNNfile=\"$(EVALFILE)\"

SUFFIX := .exe

OUT := $(EXE)$(SUFFIX)


$(EXE): $(SOURCES)
	$(CXX) $^ $(CXXFLAGS) -o $(OUT)
