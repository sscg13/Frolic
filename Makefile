EXE := Frolic
EVALFILE := chess-net15.nnue
ARCH := native

SOURCES := Frolic.cpp

CXX := clang++

CXXFLAGS := -O3 -march=$(ARCH) -static -pthread -DEUNNfile=\"$(EVALFILE)\"

DEBUGFLAGS := -g -march=$(ARCH) -static -pthread -DEUNNfile=\"$(EVALFILE)\"

SUFFIX :=

ifeq ($(OS), Windows_NT)
	SUFFIX := .exe
endif

OUT := $(EXE)$(SUFFIX)


$(EXE): $(SOURCES)
	$(CXX) $^ $(CXXFLAGS) -o $(OUT)

debug: $(SOURCES)
	$(CXX) $^ $(DEBUGFLAGS) -o debug$(SUFFIX)