#include <cmath>
#include <fstream>
#define INCBIN_PREFIX
#include "incbin.h"

const int nnuesize = 64;
const int outputbuckets = 8;
const int evalscale = 400;
const int evalQA = 255;
const int evalQB = 64;
const int material[6] = {1, 1, 1, 1, 1, 0};
const int bucketdivisor = 32 / outputbuckets;
const int nnuefilesize =
    (1538 * nnuesize + 4 * nnuesize * outputbuckets + 2 * outputbuckets);
INCBIN(char, NNUE, EUNNfile);
int screlu(short int x) {
  return std::pow(std::max(std::min((int)x, 255), 0), 2);
}
class NNUE {
  short int nnuelayer1[768][nnuesize];
  short int layer1bias[nnuesize];
  int ourlayer2[outputbuckets][nnuesize];
  int theirlayer2[outputbuckets][nnuesize];
  short int accumulation[128][nnuesize];
  int finalbias[outputbuckets];
  int totalmaterial;
  int ply;

public:
  void loaddefaultnet();
  const short int* layer1weights(int side, int color, int piece, int square);
  void activatepiece(int color, int piece, int square);
  void deactivatepiece(int color, int piece, int square);
  void initializennue(uint64_t *Bitboards);
  void forwardaccumulators(int notation);
  void backwardaccumulators(int notation);
  int evaluate(int color);
};

void NNUE::loaddefaultnet() {
  int offset = 0;
  for (int i = 0; i < 768; i++) {
    int piece = i / 64;
    int square = i % 64;
    for (int j = 0; j < nnuesize; j++) {
      short int weight = 256 * (short int)(NNUEData[offset + 1]) +
                         (short int)(unsigned char)(NNUEData[offset]);
      nnuelayer1[64 * piece + square][j] = weight;
      offset += 2;
    }
  }
  for (int i = 0; i < nnuesize; i++) {
    short int bias = 256 * (short int)(NNUEData[offset + 1]) +
                     (short int)(unsigned char)(NNUEData[offset]);
    layer1bias[i] = bias;
    offset += 2;
  }
  for (int j = 0; j < outputbuckets; j++) {
    for (int i = 0; i < nnuesize; i++) {
      short int weight = 256 * (short int)(NNUEData[offset + 1]) +
                         (short int)(unsigned char)(NNUEData[offset]);
      ourlayer2[j][i] = (int)weight;
      offset += 2;
    }
    for (int i = 0; i < nnuesize; i++) {
      short int weight = 256 * (short int)(NNUEData[offset + 1]) +
                         (short int)(unsigned char)(NNUEData[offset]);
      theirlayer2[j][i] = (int)weight;
      offset += 2;
    }
  }
  for (int j = 0; j < outputbuckets; j++) {
    short int base = 256 * (short int)(NNUEData[offset + 1]) +
                     (short int)(unsigned char)(NNUEData[offset]);
    finalbias[j] = base;
    offset += 2;
  }
}
const short int* NNUE::layer1weights(int side, int color, int piece, int square) {
  int featureindex = 64 * (6 * (color ^ side) + piece) + (56 * side) ^ square;
  return nnuelayer1[featureindex];
}
void NNUE::activatepiece(int color, int piece, int square) {
  for (int side = 0; side < 2; side++) {
    short int* accptr = accumulation[2 * ply + side];
    const short int* weightsptr = layer1weights(side, color, piece, square);
    for (int i = 0; i < nnuesize; i++) {
      accptr[i] += weightsptr[i];
    }
  }
}
void NNUE::deactivatepiece(int color, int piece, int square) {
  for (int side = 0; side < 2; side++) {
    short int* accptr = accumulation[2 * ply + side];
    const short int* weightsptr = layer1weights(side, color, piece, square);
    for (int i = 0; i < nnuesize; i++) {
      accptr[i] -= weightsptr[i];
    }
  }
}
void NNUE::initializennue(uint64_t *Bitboards) {
  totalmaterial = 0;
  ply = 0;
  for (int i = 0; i < nnuesize; i++) {
    accumulation[0][i] = layer1bias[i];
    accumulation[1][i] = layer1bias[i];
  }
  for (int i = 0; i < 12; i++) {
    uint64_t pieces = (Bitboards[i / 6] & Bitboards[2 + (i % 6)]);
    int piececount = __builtin_popcountll(pieces);
    for (int j = 0; j < piececount; j++) {
      int square = __builtin_ctzll(pieces);
      activatepiece(i / 6, i % 6, square);
      pieces ^= (1ULL << square);
    }
    totalmaterial += piececount * material[i % 6];
  }
}
void NNUE::forwardaccumulators(int notation) {
  int from = notation & 63;
  int to = (notation >> 6) & 63;
  int color = (notation >> 12) & 1;
  int piece = (notation >> 13) & 7;
  int epcapture = (notation >> 25) & 1;
  int captured = epcapture ? 2 : ((notation >> 17) & 7);
  int promoted = (notation >> 21) & 3;
  int castling = (notation >> 23) & 1;
  int from1 = (to & 4) ? to + 1 : to - 2;
  int to1 = (to & 4) ? to - 1 : to + 1;
  int to2 = epcapture ? (to + 16 * color - 8) : to;
  int piece2 = (promoted > 0) ? promoted + 1 : piece - 2;
  for (int side = 0; side < 2; side++) {
    short int *newaccptr = accumulation[2 * (ply + 1) + side];
    const short int *oldaccptr = accumulation[2 * ply + side];
    const short int *addweightsptr = layer1weights(side, color, piece2, to);
    const short int *subweightsptr = layer1weights(side, color, piece - 2, from);
    for (int i = 0; i < nnuesize; i++) {
      newaccptr[i] = oldaccptr[i] + addweightsptr[i] - subweightsptr[i];
    }
  }
  ply++;
  if (captured > 0) {
    deactivatepiece(color ^ 1, captured - 2, to2);
    totalmaterial -= material[captured - 2];
  }
  if (castling > 0) {
    activatepiece(color, 3, to1);
    deactivatepiece(color, 3, from1);
  }
}
void NNUE::backwardaccumulators(int notation) {
  int epcapture = (notation >> 25) & 1;
  int captured = epcapture ? 2 : ((notation >> 17) & 7);
  if (captured > 0) {
    totalmaterial += material[captured - 2];
  }
  ply--;
}
int NNUE::evaluate(int color) {
  int bucket = std::min(totalmaterial / bucketdivisor, outputbuckets - 1);
  int eval = finalbias[bucket] * evalQA;
  short int *stmaccptr = accumulation[2 * ply + color];
  short int *nstmaccptr = accumulation[2 * ply + (color ^ 1)];
  const int *stmweightsptr = ourlayer2[bucket];
  const int *nstmweightsptr = theirlayer2[bucket];
  for (int i = 0; i < nnuesize; i++) {
    eval += screlu(stmaccptr[i]) * stmweightsptr[i];
  }
  for (int i = 0; i < nnuesize; i++) {
    eval += screlu(nstmaccptr[i]) * nstmweightsptr[i];
  }
  eval /= evalQA;
  eval *= evalscale;
  eval /= (evalQA * evalQB);
  return eval;
}
