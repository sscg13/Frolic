#include <fstream>
const int nnuesize = 64;
int screlu(short int x) { return pow(std::max(std::min((int)x, 255), 0), 2); }
class NNUE {
  short int nnuelayer1[768][nnuesize];
  short int layer1bias[nnuesize];
  int ourlayer2[nnuesize];
  int theirlayer2[nnuesize];
  short int whitehidden[nnuesize];
  short int blackhidden[nnuesize];
  int finalbias;
  int evalscale = 400;
  int evalQA = 255;
  int evalQB = 64;

public:
  void readnnuefile(std::string file);
  void activatepiece(int color, int piece, int square);
  void deactivatepiece(int color, int piece, int square);
  void initializennue(uint64_t *Bitboards);
  void forwardaccumulators(int notation);
  void backwardaccumulators(int notation);
  int evaluate(int color);
};

void NNUE::readnnuefile(std::string file) {
  std::ifstream nnueweights;
  nnueweights.open(file, std::ifstream::binary);
  for (int i = 0; i < 768; i++) {
    int piece = i / 64;
    int square = i % 64;
    int convert[12] = {0, 3, 1, 4, 2, 5, 6, 9, 7, 10, 8, 11};
    for (int j = 0; j < nnuesize; j++) {
      char *weights = new char[2];
      nnueweights.read(weights, 2);
      short int weight = 256 * (short int)(weights[1]) +
                         (short int)(unsigned char)(weights[0]);
      nnuelayer1[64 * convert[piece] + square][j] = weight;
      delete[] weights;
    }
  }
  for (int i = 0; i < nnuesize; i++) {
    char *biases = new char[2];
    nnueweights.read(biases, 2);
    short int bias =
        256 * (short int)(biases[1]) + (short int)(unsigned char)(biases[0]);
    layer1bias[i] = bias;
    delete[] biases;
  }
  for (int i = 0; i < nnuesize; i++) {
    char *weights = new char[2];
    nnueweights.read(weights, 2);
    short int weight =
        256 * (short int)(weights[1]) + (short int)(unsigned char)(weights[0]);
    ourlayer2[i] = (int)weight;
    delete[] weights;
  }
  for (int i = 0; i < nnuesize; i++) {
    char *weights = new char[2];
    nnueweights.read(weights, 2);
    short int weight =
        256 * (short int)(weights[1]) + (short int)(unsigned char)(weights[0]);
    theirlayer2[i] = (int)weight;
    delete[] weights;
  }
  char *bases = new char[2];
  nnueweights.read(bases, 2);
  short int base =
      256 * (short int)(bases[1]) + (short int)(unsigned char)(bases[0]);
  finalbias = base;
  delete[] bases;
  nnueweights.close();
}
void NNUE::activatepiece(int color, int piece, int square) {
  for (int i = 0; i < nnuesize; i++) {
    whitehidden[i] += nnuelayer1[64 * (6 * color + piece) + square][i];
    blackhidden[i] +=
        nnuelayer1[64 * (6 * (color ^ 1) + piece) + 56 ^ square][i];
  }
}
void NNUE::deactivatepiece(int color, int piece, int square) {
  for (int i = 0; i < nnuesize; i++) {
    whitehidden[i] -= nnuelayer1[64 * (6 * color + piece) + square][i];
    blackhidden[i] -=
        nnuelayer1[64 * (6 * (color ^ 1) + piece) + 56 ^ square][i];
  }
}
void NNUE::initializennue(uint64_t *Bitboards) {
  for (int i = 0; i < nnuesize; i++) {
    whitehidden[i] = layer1bias[i];
    blackhidden[i] = layer1bias[i];
  }
  for (int i = 0; i < 12; i++) {
    uint64_t pieces = (Bitboards[i / 6] & Bitboards[2 + (i % 6)]);
    int piececount = __builtin_popcountll(pieces);
    for (int j = 0; j < piececount; j++) {
      int square = __builtin_popcountll((pieces & -pieces) - 1);
      activatepiece(i / 6, i % 6, square);
      pieces ^= (1ULL << square);
    }
  }
}
void NNUE::forwardaccumulators(int notation) {
  int from = notation & 63;
  int to = (notation >> 6) & 63;
  int color = (notation >> 12) & 1;
  int piece = (notation >> 13) & 7;
  int captured = (notation >> 17) & 7;
  int promoted = (notation >> 21) & 3;
  int piece2 = (promoted > 0) ? piece : piece - 2;
  NNUE::activatepiece(color, piece2, to);
  NNUE::deactivatepiece(color, piece - 2, from);
  if (captured > 0) {
    NNUE::deactivatepiece(color ^ 1, captured - 2, to);
  }
}
void NNUE::backwardaccumulators(int notation) {
  int from = notation & 63;
  int to = (notation >> 6) & 63;
  int color = (notation >> 12) & 1;
  int piece = (notation >> 13) & 7;
  int captured = (notation >> 17) & 7;
  int promoted = (notation >> 21) & 3;
  int piece2 = promoted ? piece : piece - 2;
  NNUE::deactivatepiece(color, piece2, to);
  NNUE::activatepiece(color, piece - 2, from);
  if (captured > 0) {
    NNUE::activatepiece(color ^ 1, captured - 2, to);
  }
}
int NNUE::evaluate(int color) {
  int eval = finalbias * evalQA;
  if (color == 0) {
    for (int i = 0; i < nnuesize; i++) {
      eval += screlu(whitehidden[i]) * ourlayer2[i];
      eval += screlu(blackhidden[i]) * theirlayer2[i];
    }
  } else {
    for (int i = 0; i < nnuesize; i++) {
      eval += screlu(whitehidden[i]) * theirlayer2[i];
      eval += screlu(blackhidden[i]) * ourlayer2[i];
    }
  }
  eval /= evalQA;
  eval *= evalscale;
  eval /= (evalQA * evalQB);
  return eval;
}
