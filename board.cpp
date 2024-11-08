#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <random>
#include <string>
using U64 = uint64_t;

U64 FileA = 0x0101010101010101;
U64 FileB = FileA << 1;
U64 FileC = FileA << 2;
U64 FileD = FileA << 3;
U64 FileE = FileA << 4;
U64 FileF = FileA << 5;
U64 FileG = FileA << 6;
U64 FileH = FileA << 7;
U64 Rank1 = 0xFF;
U64 Rank2 = Rank1 << 8;
U64 Rank3 = Rank1 << 16;
U64 Rank4 = Rank1 << 24;
U64 Rank5 = Rank1 << 32;
U64 Rank6 = Rank1 << 40;
U64 Rank7 = Rank1 << 48;
U64 Rank8 = Rank1 << 56;
// White, Black, Pawn, Knight, Bishop, Rook, Queen, King
U64 AntiDiags[15] = {
    0x0000000000000001, 0x0000000000000102, 0x0000000000010204,
    0x0000000001020408, 0x0000000102040810, 0x0000010204081020,
    0x0001020408102040, 0x0102040810204080, 0x0204081020408000,
    0x0408102040800000, 0x0810204080000000, 0x1020408000000000,
    0x2040800000000000, 0x4080000000000000, 0x8000000000000000};
U64 Diags[15] = {0x0100000000000000, 0x0201000000000000, 0x0402010000000000,
                 0x0804020100000000, 0x1008040201000000, 0x2010080402010000,
                 0x4020100804020100, 0x8040201008040201, 0x0080402010080402,
                 0x0000804020100804, 0x0000008040201008, 0x0000000080402010,
                 0x0000000000804020, 0x0000000000008040, 0x0000000000000080};
U64 Files[8] = {FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH};
U64 Ranks[8] = {Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8};
U64 KingAttacks[64];
U64 PawnAttacks[2][64];
U64 KnightAttacks[64];
U64 RankMask[64];
U64 FileMask[64];
U64 DiagMask[64];
U64 AntiMask[64];
U64 RankAttacks[512];
U64 hashes[8][64];
U64 epfilehash[8];
U64 colorhash = 0xE344F58E0F3B26E5;
U64 castlinghash[16];
// 1 bit color, 7 bits halfmove, 6 bits ep, 4 bits castling KQkq
// 6 bits from square, 6 bits to square, 1 bit color, 3 bits piece moved, 1 bit
// castling, 1 bit double pawn push, 1 bit en passant, 1 bit promotion, 2 bits
// promoted piece, 1 bit capture, 3 bits piece captured 26 bits total for now?
const int castlechange[64] = {
    13, 15, 15, 15, 12, 15, 15, 14, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 7,  15, 15, 15, 3,  15, 15, 11};
const int phase[6] = {0, 1, 1, 2, 4, 0};
U64 shift_w(U64 bitboard) { return (bitboard & ~FileA) >> 1; }
U64 shift_n(U64 bitboard) { return bitboard << 8; }
U64 shift_s(U64 bitboard) { return bitboard >> 8; }
U64 shift_e(U64 bitboard) { return (bitboard & ~FileH) << 1; }
void initializeleaperattacks() {
  for (int i = 0; i < 64; i++) {
    U64 square = 1ULL << i;
    PawnAttacks[0][i] = ((square & ~FileA) << 7) | ((square & ~FileH) << 9);
    PawnAttacks[1][i] = ((square & ~FileA) >> 9) | ((square & ~FileH) >> 7);
    U64 horizontal = square | shift_w(square) | shift_e(square);
    U64 full = horizontal | shift_n(horizontal) | shift_s(horizontal);
    KingAttacks[i] = full & ~square;
    U64 knightattack = ((square & ~FileA) << 15);
    knightattack |= ((square & ~FileA) >> 17);
    knightattack |= ((square & ~FileH) << 17);
    knightattack |= ((square & ~FileH) >> 15);
    knightattack |= ((square & ~FileG & ~FileH) << 10);
    knightattack |= ((square & ~FileG & ~FileH) >> 6);
    knightattack |= ((square & ~FileA & ~FileB) << 6);
    knightattack |= ((square & ~FileA & ~FileB) >> 10);
    KnightAttacks[i] = knightattack;
  }
}
void initializemasks() {
  for (int i = 0; i < 8; i++) {
    for (int j = 0; j < 8; j++) {
      RankMask[8 * i + j] = Ranks[i];
      FileMask[8 * i + j] = Files[j];
      AntiMask[8 * i + j] = AntiDiags[i + j];
      DiagMask[8 * i + j] = Diags[j + 7 - i];
    }
  }
}
void initializerankattacks() {
  for (U64 i = 0ULL; i < 0x000000000040; i++) {
    U64 occupied = i << 1;
    for (int j = 0; j < 8; j++) {
      U64 attacks = 0ULL;
      if (j > 0) {
        int k = j - 1;
        while (k >= 0) {
          attacks |= (1ULL << k);
          if ((1ULL << k) & occupied) {
            k = 0;
          }
          k--;
        }
      }
      if (j < 7) {
        int k = j + 1;
        while (k <= 7) {
          attacks |= (1ULL << k);
          if ((1ULL << k) & occupied) {
            k = 7;
          }
          k++;
        }
      }
      RankAttacks[8 * i + j] = attacks;
    }
  }
}
U64 PawnMoves(U64 occupied, int square, int color) {
  int row = square & 56;
  U64 step1;
  if (color == 0) {
    step1 = (1ULL << (square + 8)) & (~occupied);
  } else {
    step1 = (1ULL << (square - 8)) & (~occupied);
  }
  if ((row != 8 && color == 0) || (row != 48 && color == 1)) {
    return step1;
  }
  U64 step2;
  if (color == 0) {
    step2 = (step1 << 8) & (~occupied);
  } else {
    step2 = (step1 >> 8) & (~occupied);
  }
  return step1 | step2;
}
U64 DiagAttacks(U64 occupied, int square) {
  U64 forwards = occupied & DiagMask[square];
  U64 backwards = __builtin_bswap64(forwards);
  forwards = forwards - 2 * (1ULL << square);
  backwards = backwards - 2 * (1ULL << (56 ^ square));
  backwards = __builtin_bswap64(backwards);
  return (forwards ^ backwards) & DiagMask[square];
}
U64 AntiAttacks(U64 occupied, int square) {
  U64 forwards = occupied & AntiMask[square];
  U64 backwards = __builtin_bswap64(forwards);
  forwards = forwards - 2 * (1ULL << square);
  backwards = backwards - 2 * (1ULL << (56 ^ square));
  backwards = __builtin_bswap64(backwards);
  return (forwards ^ backwards) & AntiMask[square];
}
U64 FileAttacks(U64 occupied, int square) {
  U64 forwards = occupied & FileMask[square];
  U64 backwards = __builtin_bswap64(forwards);
  forwards = forwards - 2 * (1ULL << square);
  backwards = backwards - 2 * (1ULL << (56 ^ square));
  backwards = __builtin_bswap64(backwards);
  return (forwards ^ backwards) & FileMask[square];
}
U64 GetRankAttacks(U64 occupied, int square) {
  int row = square & 56;
  int file = square & 7;
  int relevant = (occupied >> (row + 1)) & 63;
  return (RankAttacks[8 * relevant + file] << row);
}
void initializezobrist() {
  std::mt19937_64 mt(20346892);
  for (int i = 0; i < 8; i++) {
    for (int j = 0; j < 64; j++) {
      hashes[i][j] = mt();
    }
    epfilehash[i] = mt();
  }
  for (int i = 0; i < 16; i++) {
    castlinghash[i] = mt();
  }
}
std::string coordinate(int square) {
  std::string convert[64] = {
      "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "a2", "b2", "c2",
      "d2", "e2", "f2", "g2", "h2", "a3", "b3", "c3", "d3", "e3", "f3",
      "g3", "h3", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "a5",
      "b5", "c5", "d5", "e5", "f5", "g5", "h5", "a6", "b6", "c6", "d6",
      "e6", "f6", "g6", "h6", "a7", "b7", "c7", "d7", "e7", "f7", "g7",
      "h7", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"};
  return convert[square];
}
std::string algebraic(int notation) {
  std::string convert[64] = {
      "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "a2", "b2", "c2",
      "d2", "e2", "f2", "g2", "h2", "a3", "b3", "c3", "d3", "e3", "f3",
      "g3", "h3", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "a5",
      "b5", "c5", "d5", "e5", "f5", "g5", "h5", "a6", "b6", "c6", "d6",
      "e6", "f6", "g6", "h6", "a7", "b7", "c7", "d7", "e7", "f7", "g7",
      "h7", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"};
  std::string header = convert[notation & 63] + convert[(notation >> 6) & 63];
  if (notation & (1 << 20)) {
    int piece = (notation >> 21) & 3;
    if (piece == 0) {
      header = header + "n";
    } else if (piece == 1) {
      header = header + "b";
    } else if (piece == 2) {
      header = header + "r";
    } else {
      header = header + "q";
    }
  }
  return header;
}
class Board {
  U64 zobrist[1024];
  int history[1024];
  int last = 0;
  int root = 0;
  const int startpiece[16] = {3, 1, 2, 4, 5, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0};

public:
  U64 Bitboards[8];
  int gamelength = 0;
  int position = 0;
  int nodecount = 0;
  int moves[64][256];
  int movescore[64][256];
  int gamephase[2] = {0, 0};
  U64 zobristhash = 0ULL;
  U64 scratchzobrist();
  void initialize();
  int repetitions();
  int halfmovecount();
  bool insufficientmaterial();
  U64 checkers(int color);
  void makenullmove();
  void unmakenullmove();
  void makemove(int notation, bool reversible);
  void unmakemove(int notation);
  int generatemoves(int color, bool capturesonly, int depth);
  U64 perft(int depth, int initialdepth, int color);
  U64 perftnobulk(int depth, int initialdepth, int color);
  void parseFEN(std::string FEN);
  std::string getFEN();
  bool see_exceeds(int mov, int color, int threshold);
};

U64 Board::scratchzobrist() {
  U64 scratch = 0ULL;
  for (int i = 0; i < 8; i++) {
    for (int j = 0; j < 64; j++) {
      if (Bitboards[i] & (1ULL << j)) {
        scratch ^= hashes[i][j];
      }
    }
  }
  if (position & 1) {
    scratch ^= colorhash;
  }
  if (position & 0x00003F00) {
    int file = (position >> 8) & 7;
    scratch ^= epfilehash[file];
  }
  int castling = (position >> 14) & 15;
  scratch ^= castlinghash[castling];
  return scratch;
}
void Board::initialize() {
  Bitboards[0] = Rank1 | Rank2;
  Bitboards[1] = Rank7 | Rank8;
  Bitboards[2] = Rank2 | Rank7;
  Bitboards[3] = (Rank1 | Rank8) & (FileB | FileG);
  Bitboards[4] = (Rank1 | Rank8) & (FileC | FileF);
  Bitboards[5] = (Rank1 | Rank8) & (FileA | FileH);
  Bitboards[6] = (Rank1 | Rank8) & FileD;
  Bitboards[7] = (Rank1 | Rank8) & FileE;
  position = 0x0003C000;
  history[0] = position;
  gamephase[0] = 12;
  gamephase[1] = 12;
  gamelength = 0;
  zobrist[0] = scratchzobrist();
}
int Board::repetitions() {
  int repeats = 0;
  for (int i = gamelength - 2; i >= last; i -= 2) {
    if (zobrist[i] == zobrist[gamelength]) {
      repeats++;
      if (i >= root) {
        repeats++;
      }
    }
  }
  return repeats;
}
int Board::halfmovecount() { return (position >> 1) & 127; }
bool Board::insufficientmaterial() {
  U64 majors = Bitboards[2] | Bitboards[5] | Bitboards[6];
  if (majors) {
    return false;
  }
  U64 minors = Bitboards[3] | Bitboards[4];
  return minors & (minors - 1);
}
U64 Board::checkers(int color) {
  int kingsquare = __builtin_popcountll((Bitboards[color] & Bitboards[7]) - 1);
  int opposite = color ^ 1;
  U64 attacks = 0ULL;
  U64 occupied = Bitboards[0] | Bitboards[1];
  attacks |= (KnightAttacks[kingsquare] & Bitboards[3]);
  attacks |= (PawnAttacks[color][kingsquare] & Bitboards[2]);
  attacks |=
      (DiagAttacks(occupied, kingsquare) & (Bitboards[4] | Bitboards[6]));
  attacks |=
      (AntiAttacks(occupied, kingsquare) & (Bitboards[4] | Bitboards[6]));
  attacks |=
      (GetRankAttacks(occupied, kingsquare) & (Bitboards[5] | Bitboards[6]));
  attacks |=
      (FileAttacks(occupied, kingsquare) & (Bitboards[5] | Bitboards[6]));
  attacks &= Bitboards[opposite];
  return attacks;
}
void Board::makenullmove() {
  gamelength++;
  int halfmove = (position >> 1) & 127;
  if (position & 0x00003F00) {
    int file = (position >> 8) & 7;
    zobristhash ^= epfilehash[file];
    position ^= ((position >> 8) & 63);
  }
  zobristhash ^= colorhash;
  position ^= (halfmove << 1);
  halfmove++;
  position ^= (halfmove << 1);
  position ^= 1;
  zobrist[gamelength] = zobristhash;
  history[gamelength] = position;
}
void Board::unmakenullmove() {
  gamelength--;
  position = history[gamelength];
  zobristhash = zobrist[gamelength];
}
void Board::makemove(int notation, bool reversible) {
  // 6 bits from square, 6 bits to square, 1 bit color, 3 bits piece moved, 1
  // bit capture, 3 bits piece captured, 1 bit promotion, 2 bits promoted piece,
  // 1 bit castling, 1 bit double pawn push, 1 bit en passant, 26 bits total

  // 1 bit color, 7 bits halfmove, 6 bits ep, 4 bits castling
  gamelength++;
  if (!reversible) {
    root = gamelength;
  }
  if (position & 0x00003F00) {
    int file = (position >> 8) & 7;
    zobristhash ^= epfilehash[file];
  }
  int castling = (position >> 14) & 15;
  zobristhash ^= castlinghash[castling];
  int from = notation & 63;
  int to = (notation >> 6) & 63;
  int color = (notation >> 12) & 1;
  int piece = (notation >> 13) & 7;
  Bitboards[color] ^= ((1ULL << from) | (1ULL << to));
  Bitboards[piece] ^= ((1ULL << from) | (1ULL << to));
  zobristhash ^= (hashes[color][from] ^ hashes[color][to]);
  zobristhash ^= (hashes[piece][from] ^ hashes[piece][to]);
  int captured = (notation >> 17) & 7;
  int promoted = (notation >> 21) & 3;
  int halfmove = (position >> 1) & 127;
  position ^= (halfmove << 1);
  halfmove++;
  position &= 0x0003C0FF;
  if (piece == 2) {
    halfmove = 0;
    if (!reversible) {
      last = gamelength;
    }
  }
  if (notation & (1 << 16)) {
    Bitboards[color ^ 1] ^= (1ULL << to);
    Bitboards[captured] ^= (1ULL << to);
    zobristhash ^= (hashes[color ^ 1][to] ^ hashes[captured][to]);
    gamephase[color ^ 1] -= phase[captured - 2];
    halfmove = 0;
    if (!reversible) {
      last = gamelength;
    }
  }
  if (notation & (1 << 20)) {
    Bitboards[2] ^= (1ULL << to);
    Bitboards[promoted + 3] ^= (1ULL << to);
    zobristhash ^= (hashes[2][to] ^ hashes[promoted + 3][to]);
    gamephase[color] += phase[promoted + 1];
  } else if (notation & (1 << 23)) {
    if (to & 4) {
      Bitboards[color] ^= ((1ULL << (to - 1)) | (1ULL << (to + 1)));
      Bitboards[5] ^= ((1ULL << (to - 1)) | (1ULL << (to + 1)));
      zobristhash ^= (hashes[color][to - 1] ^ hashes[color][to + 1]);
      zobristhash ^= (hashes[5][to - 1] ^ hashes[5][to + 1]);
    } else {
      Bitboards[color] ^= ((1ULL << (to - 2)) | (1ULL << (to + 1)));
      Bitboards[5] ^= ((1ULL << (to - 2)) | (1ULL << (to + 1)));
      zobristhash ^= (hashes[color][to - 2] ^ hashes[color][to + 1]);
      zobristhash ^= (hashes[5][to - 2] ^ hashes[5][to + 1]);
    }
  } else if (notation & (1 << 24)) {
    position ^= ((from + to) << 7);
  } else if (notation & (1 << 25)) {
    int shadow;
    if (color == 1) {
      shadow = to + 8;
    } else {
      shadow = to - 8;
    }
    Bitboards[2] ^= (1ULL << shadow);
    Bitboards[color ^ 1] ^= (1ULL << shadow);
    zobristhash ^= (hashes[2][shadow] ^ hashes[color ^ 1][shadow]);
  }
  position &= (0x00003FFF | (castlechange[from] << 14));
  position &= (0x00003FFF | (castlechange[to] << 14));
  position ^= 1;
  position ^= (halfmove << 1);
  zobristhash ^= colorhash;
  if (position & 0x00003F00) {
    int file = (position >> 8) & 7;
    zobristhash ^= epfilehash[file];
  }
  castling = (position >> 14) & 15;
  zobristhash ^= castlinghash[castling];
  history[gamelength] = position;
  zobrist[gamelength] = zobristhash;
  nodecount++;
}
void Board::unmakemove(int notation) {
  gamelength--;
  position = history[gamelength];
  zobristhash = zobrist[gamelength];
  int from = notation & 63;
  int to = (notation >> 6) & 63;
  int color = (notation >> 12) & 1;
  int piece = (notation >> 13) & 7;
  Bitboards[color] ^= ((1ULL << from) | (1ULL << to));
  Bitboards[piece] ^= ((1ULL << from) | (1ULL << to));
  int captured = (notation >> 17) & 7;
  int promoted = (notation >> 21) & 3;
  if (notation & (1 << 16)) {
    Bitboards[color ^ 1] ^= (1ULL << to);
    Bitboards[captured] ^= (1ULL << to);
    gamephase[color ^ 1] += phase[captured - 2];
  }
  if (notation & (1 << 20)) {
    Bitboards[2] ^= (1ULL << to);
    Bitboards[promoted + 3] ^= (1ULL << to);
    gamephase[color] -= phase[promoted + 1];
  } else if (notation & (1 << 23)) {
    if (to & 4) {
      Bitboards[color] ^= ((1ULL << (to - 1)) | (1ULL << (to + 1)));
      Bitboards[5] ^= ((1ULL << (to - 1)) | (1ULL << (to + 1)));
    } else {
      Bitboards[color] ^= ((1ULL << (to - 2)) | (1ULL << (to + 1)));
      Bitboards[5] ^= ((1ULL << (to - 2)) | (1ULL << (to + 1)));
    }
  } else if (notation & (1 << 25)) {
    int shadow;
    if (color) {
      shadow = to + 8;
    } else {
      shadow = to - 8;
    }
    Bitboards[2] ^= (1ULL << shadow);
    Bitboards[color ^ 1] ^= (1ULL << shadow);
  }
}
int Board::generatemoves(int color, bool capturesonly, int depth) {
  int movecount = 0;
  int kingsquare = __builtin_popcountll((Bitboards[color] & Bitboards[7]) - 1);
  int pinrank = kingsquare & 56;
  int pinfile = kingsquare & 7;
  int opposite = color ^ 1;
  U64 opponentattacks = 0ULL;
  U64 pinnedpieces = 0ULL;
  U64 checkmask = 0ULL;
  U64 preoccupied = Bitboards[0] | Bitboards[1];
  U64 kingDiag = DiagAttacks(preoccupied, kingsquare);
  U64 kingAnti = AntiAttacks(preoccupied, kingsquare);
  U64 kingRank = GetRankAttacks(preoccupied, kingsquare);
  U64 kingFile = FileAttacks(preoccupied, kingsquare);
  U64 occupied = preoccupied ^ (1ULL << kingsquare);
  U64 opponentpawns = Bitboards[opposite] & Bitboards[2];
  U64 opponentknights = Bitboards[opposite] & Bitboards[3];
  U64 opponentbishops = Bitboards[opposite] & Bitboards[4];
  U64 opponentrooks = Bitboards[opposite] & Bitboards[5];
  U64 opponentqueens = Bitboards[opposite] & Bitboards[6];
  int pawncount = __builtin_popcountll(opponentpawns);
  int knightcount = __builtin_popcountll(opponentknights);
  int bishopcount = __builtin_popcountll(opponentbishops);
  int rookcount = __builtin_popcountll(opponentrooks);
  int queencount = __builtin_popcountll(opponentqueens);
  U64 ourcaptures = 0ULL;
  U64 ourmoves = 0ULL;
  U64 ourmask = 0ULL;
  for (int i = 0; i < pawncount; i++) {
    int pawnsquare = __builtin_popcountll((opponentpawns & -opponentpawns) - 1);
    opponentattacks |= PawnAttacks[opposite][pawnsquare];
    opponentpawns ^= (1ULL << pawnsquare);
  }
  for (int i = 0; i < knightcount; i++) {
    int knightsquare =
        __builtin_popcountll((opponentknights & -opponentknights) - 1);
    opponentattacks |= KnightAttacks[knightsquare];
    opponentknights ^= (1ULL << knightsquare);
  }
  for (int i = 0; i < bishopcount; i++) {
    int bishopsquare =
        __builtin_popcountll((opponentbishops & -opponentbishops) - 1);
    U64 diag = DiagAttacks(occupied, bishopsquare);
    U64 anti = AntiAttacks(occupied, bishopsquare);
    if (!(diag & (1ULL << kingsquare))) {
      pinnedpieces |= (diag & kingDiag);
    } else {
      checkmask |= (DiagAttacks(preoccupied, bishopsquare) & kingDiag);
    }
    if (!(anti & (1ULL << kingsquare))) {
      pinnedpieces |= (anti & kingAnti);
    } else {
      checkmask |= (AntiAttacks(preoccupied, bishopsquare) & kingAnti);
    }
    opponentattacks |= (diag | anti);
    opponentbishops ^= (1ULL << bishopsquare);
  }
  for (int i = 0; i < rookcount; i++) {
    int rooksquare = __builtin_popcountll((opponentrooks & -opponentrooks) - 1);
    U64 r = GetRankAttacks(occupied, rooksquare);
    U64 file = FileAttacks(occupied, rooksquare);
    if (!(r & (1ULL << kingsquare))) {
      pinnedpieces |= (r & kingRank);
    } else {
      checkmask |= (GetRankAttacks(preoccupied, rooksquare) & kingRank);
    }
    if (!(file & (1ULL << kingsquare))) {
      pinnedpieces |= (file & kingFile);
    } else {
      checkmask |= (FileAttacks(preoccupied, rooksquare) & kingFile);
    }
    opponentattacks |= (r | file);
    opponentrooks ^= (1ULL << rooksquare);
  }
  for (int i = 0; i < queencount; i++) {
    int queensquare =
        __builtin_popcountll((opponentqueens & -opponentqueens) - 1);
    U64 diag = DiagAttacks(occupied, queensquare);
    U64 anti = AntiAttacks(occupied, queensquare);
    U64 r = GetRankAttacks(occupied, queensquare);
    U64 file = FileAttacks(occupied, queensquare);
    if (!(diag & (1ULL << kingsquare))) {
      pinnedpieces |= (diag & kingDiag);
    } else {
      checkmask |= (DiagAttacks(preoccupied, queensquare) & kingDiag);
    }
    if (!(anti & (1ULL << kingsquare))) {
      pinnedpieces |= (anti & kingAnti);
    } else {
      checkmask |= (AntiAttacks(preoccupied, queensquare) & kingAnti);
    }
    if (!(r & (1ULL << kingsquare))) {
      pinnedpieces |= (r & kingRank);
    } else {
      checkmask |= (GetRankAttacks(preoccupied, queensquare) & kingRank);
    }
    if (!(file & (1ULL << kingsquare))) {
      pinnedpieces |= (file & kingFile);
    } else {
      checkmask |= (FileAttacks(preoccupied, queensquare) & kingFile);
    }
    opponentattacks |= (r | file);
    opponentattacks |= (diag | anti);
    opponentqueens ^= (1ULL << queensquare);
  }
  int opponentking =
      __builtin_popcountll((Bitboards[opposite] & Bitboards[7]) - 1);
  opponentattacks |= KingAttacks[opponentking];
  ourcaptures =
      KingAttacks[kingsquare] & ((~opponentattacks) & Bitboards[opposite]);
  int capturenumber = __builtin_popcountll(ourcaptures);
  int movenumber;
  for (int i = 0; i < capturenumber; i++) {
    int capturesquare = __builtin_popcountll((ourcaptures & -ourcaptures) - 1);
    int notation = kingsquare | (capturesquare << 6);
    notation |= (color << 12);
    notation |= (7 << 13);
    int captured = 0;
    for (int j = 2; j < 7; j++) {
      if ((1ULL << capturesquare) & (Bitboards[opposite] & Bitboards[j])) {
        captured = j;
      }
    }
    notation |= (1 << 16);
    notation |= (captured << 17);
    moves[depth][movecount] = notation;
    movecount++;
    ourcaptures ^= (1ULL << capturesquare);
  }
  if (!capturesonly) {
    ourmoves = KingAttacks[kingsquare] & ((~opponentattacks) & (~preoccupied));
    movenumber = __builtin_popcountll(ourmoves);
    for (int i = 0; i < movenumber; i++) {
      int movesquare = __builtin_popcountll((ourmoves & -ourmoves) - 1);
      int notation = kingsquare | (movesquare << 6);
      notation |= (color << 12);
      notation |= (7 << 13);
      moves[depth][movecount] = notation;
      movecount++;
      ourmoves ^= (1ULL << movesquare);
    }
  }
  U64 checks = checkers(color);
  if (__builtin_popcountll(checks) > 1) {
    return movecount;
  } else if (checks) {
    checkmask |= checks;
  } else {
    checkmask = ~(0ULL);
  }
  U64 ourpawns = Bitboards[color] & Bitboards[2];
  U64 ourknights = Bitboards[color] & Bitboards[3];
  U64 ourbishops = Bitboards[color] & Bitboards[4];
  U64 ourrooks = Bitboards[color] & Bitboards[5];
  U64 ourqueens = Bitboards[color] & Bitboards[6];
  pawncount = __builtin_popcountll(ourpawns);
  knightcount = __builtin_popcountll(ourknights);
  bishopcount = __builtin_popcountll(ourbishops);
  rookcount = __builtin_popcountll(ourrooks);
  queencount = __builtin_popcountll(ourqueens);
  for (int i = 0; i < pawncount; i++) {
    int pawnsquare = __builtin_popcountll((ourpawns & -ourpawns) - 1);
    int epsquare = (position >> 8) & 63;
    U64 pinmask = ~(0ULL);
    if (pinnedpieces & (1ULL << pawnsquare)) {
      int pawnrank = pawnsquare & 56;
      int pawnfile = pawnsquare & 7;
      if (pawnrank == pinrank) {
        pinmask = GetRankAttacks(preoccupied, pawnsquare);
      } else if (pawnfile == pinfile) {
        pinmask = FileAttacks(preoccupied, pawnsquare);
      } else if ((pawnfile - pinfile) * (pawnrank - pinrank) > 0) {
        pinmask = DiagAttacks(preoccupied, pawnsquare);
      } else {
        pinmask = AntiAttacks(preoccupied, pawnsquare);
      }
    }
    ourcaptures = PawnAttacks[color][pawnsquare] &
                  (Bitboards[opposite] | (((1ULL << epsquare) >> 1) << 1));
    int eptake;
    if (color == 0) {
      eptake = epsquare - 8;
    } else {
      eptake = epsquare + 8;
    }
    U64 effectivemask = checkmask;
    if (checkmask & (1ULL << eptake)) {
      effectivemask |= (1ULL << epsquare);
    }
    ourcaptures &= (pinmask & effectivemask);
    int capturenumber = __builtin_popcountll(ourcaptures);
    for (int j = 0; j < capturenumber; j++) {
      int capturesquare =
          __builtin_popcountll((ourcaptures & -ourcaptures) - 1);
      int notation = pawnsquare | (capturesquare << 6);
      notation |= (color << 12);
      notation |= (2 << 13);
      int captured = 0;
      bool legal = true;
      for (int j = 2; j < 7; j++) {
        if ((1ULL << capturesquare) & (Bitboards[opposite] & Bitboards[j])) {
          captured = j;
        }
      }
      if (captured > 0) {
        notation |= (1 << 16);
        notation |= (captured << 17);
      } else {
        notation |= (1 << 25);
        if (GetRankAttacks(
                (preoccupied ^ ((1ULL << eptake) | (1ULL << pawnsquare))),
                kingsquare) &
            (Bitboards[opposite] & (Bitboards[5] | Bitboards[6]))) {
          legal = false;
        }
      }
      if (((color == 0) && (capturesquare & 56) == 56) ||
          ((color == 1) && (capturesquare & 56) == 0)) {
        for (int k = 3; k >= 0; k--) {
          moves[depth][movecount] = notation | ((1 << 20) | (k << 21));
          movecount++;
        }
      } else if (legal) {
        moves[depth][movecount] = notation;
        movecount++;
      }
      ourcaptures ^= (1ULL << capturesquare);
    }
    if (!capturesonly) {
      ourmoves = PawnMoves(preoccupied, pawnsquare, color);
      ourmoves &= (pinmask & checkmask);
      int movenumber = __builtin_popcountll(ourmoves);
      for (int j = 0; j < movenumber; j++) {
        int movesquare = __builtin_popcountll((ourmoves & -ourmoves) - 1);
        int notation = pawnsquare | (movesquare << 6);
        notation |= (color << 12);
        notation |= (2 << 13);
        if ((movesquare - pawnsquare == 16 && color == 0) ||
            (pawnsquare - movesquare == 16 && color == 1)) {
          notation |= (1 << 24);
        }
        if (((color == 0) && (movesquare & 56) == 56) ||
            ((color == 1) && (movesquare & 56) == 0)) {
          for (int k = 3; k >= 0; k--) {
            moves[depth][movecount] = notation | ((1 << 20) | (k << 21));
            movecount++;
          }
        } else {
          moves[depth][movecount] = notation;
          movecount++;
        }
        ourmoves ^= (1ULL << movesquare);
      }
    }
    ourpawns ^= (1ULL << pawnsquare);
  }
  for (int i = 0; i < knightcount; i++) {
    int knightsquare = __builtin_popcountll((ourknights & -ourknights) - 1);
    U64 pinmask = ~(0ULL);
    if (pinnedpieces & (1ULL << knightsquare)) {
      int knightrank = knightsquare & 56;
      int knightfile = knightsquare & 7;
      if (knightrank == pinrank) {
        pinmask = GetRankAttacks(preoccupied, knightsquare);
      } else if (knightfile == pinfile) {
        pinmask = FileAttacks(preoccupied, knightsquare);
      } else if ((knightfile - pinfile) * (knightrank - pinrank) > 0) {
        pinmask = DiagAttacks(preoccupied, knightsquare);
      } else {
        pinmask = AntiAttacks(preoccupied, knightsquare);
      }
    }
    ourmask = KnightAttacks[knightsquare];
    ourmask &= (pinmask & checkmask);
    ourcaptures = ourmask & Bitboards[opposite];
    int capturenumber = __builtin_popcountll(ourcaptures);
    for (int j = 0; j < capturenumber; j++) {
      int capturesquare =
          __builtin_popcountll((ourcaptures & -ourcaptures) - 1);
      int notation = knightsquare | (capturesquare << 6);
      notation |= (color << 12);
      notation |= (3 << 13);
      int captured = 0;
      for (int j = 2; j < 7; j++) {
        if ((1ULL << capturesquare) & (Bitboards[opposite] & Bitboards[j])) {
          captured = j;
        }
      }
      notation |= (1 << 16);
      notation |= (captured << 17);
      moves[depth][movecount] = notation;
      movecount++;
      ourcaptures ^= (1ULL << capturesquare);
    }
    if (!capturesonly) {
      ourmoves = ourmask & (~preoccupied);
      int movenumber = __builtin_popcountll(ourmoves);
      for (int j = 0; j < movenumber; j++) {
        int movesquare = __builtin_popcountll((ourmoves & -ourmoves) - 1);
        int notation = knightsquare | (movesquare << 6);
        notation |= (color << 12);
        notation |= (3 << 13);
        moves[depth][movecount] = notation;
        movecount++;
        ourmoves ^= (1ULL << movesquare);
      }
    }
    ourknights ^= (1ULL << knightsquare);
  }
  for (int i = 0; i < bishopcount; i++) {
    int bishopsquare = __builtin_popcountll((ourbishops & -ourbishops) - 1);
    U64 pinmask = ~(0ULL);
    if (pinnedpieces & (1ULL << bishopsquare)) {
      int bishoprank = bishopsquare & 56;
      int bishopfile = bishopsquare & 7;
      if (bishoprank == pinrank) {
        pinmask = GetRankAttacks(preoccupied, bishopsquare);
      } else if (bishopfile == pinfile) {
        pinmask = FileAttacks(preoccupied, bishopsquare);
      } else if ((bishopfile - pinfile) * (bishoprank - pinrank) > 0) {
        pinmask = DiagAttacks(preoccupied, bishopsquare);
      } else {
        pinmask = AntiAttacks(preoccupied, bishopsquare);
      }
    }
    ourmask = (DiagAttacks(preoccupied, bishopsquare) |
               AntiAttacks(preoccupied, bishopsquare));
    ourmask &= (pinmask & checkmask);
    ourcaptures = ourmask & Bitboards[opposite];
    int capturenumber = __builtin_popcountll(ourcaptures);
    for (int j = 0; j < capturenumber; j++) {
      int capturesquare =
          __builtin_popcountll((ourcaptures & -ourcaptures) - 1);
      int notation = bishopsquare | (capturesquare << 6);
      notation |= (color << 12);
      notation |= (4 << 13);
      int captured = 0;
      for (int j = 2; j < 7; j++) {
        if ((1ULL << capturesquare) & (Bitboards[opposite] & Bitboards[j])) {
          captured = j;
        }
      }
      notation |= (1 << 16);
      notation |= (captured << 17);
      moves[depth][movecount] = notation;
      movecount++;
      ourcaptures ^= (1ULL << capturesquare);
    }
    if (!capturesonly) {
      ourmoves = ourmask & (~preoccupied);
      int movenumber = __builtin_popcountll(ourmoves);
      for (int j = 0; j < movenumber; j++) {
        int movesquare = __builtin_popcountll((ourmoves & -ourmoves) - 1);
        int notation = bishopsquare | (movesquare << 6);
        notation |= (color << 12);
        notation |= (4 << 13);
        moves[depth][movecount] = notation;
        movecount++;
        ourmoves ^= (1ULL << movesquare);
      }
    }
    ourbishops ^= (1ULL << bishopsquare);
  }
  for (int i = 0; i < rookcount; i++) {
    int rooksquare = __builtin_popcountll((ourrooks & -ourrooks) - 1);
    U64 pinmask = ~(0ULL);
    if (pinnedpieces & (1ULL << rooksquare)) {
      int rookrank = rooksquare & 56;
      int rookfile = rooksquare & 7;
      if (rookrank == pinrank) {
        pinmask = GetRankAttacks(preoccupied, rooksquare);
      } else if (rookfile == pinfile) {
        pinmask = FileAttacks(preoccupied, rooksquare);
      } else if ((rookfile - pinfile) * (rookrank - pinrank) > 0) {
        pinmask = DiagAttacks(preoccupied, rooksquare);
      } else {
        pinmask = AntiAttacks(preoccupied, rooksquare);
      }
    }
    ourmask = (GetRankAttacks(preoccupied, rooksquare) |
               FileAttacks(preoccupied, rooksquare));
    ourmask &= (pinmask & checkmask);
    ourcaptures = ourmask & Bitboards[opposite];
    int capturenumber = __builtin_popcountll(ourcaptures);
    for (int j = 0; j < capturenumber; j++) {
      int capturesquare =
          __builtin_popcountll((ourcaptures & -ourcaptures) - 1);
      int notation = rooksquare | (capturesquare << 6);
      notation |= (color << 12);
      notation |= (5 << 13);
      int captured = 0;
      for (int j = 2; j < 7; j++) {
        if ((1ULL << capturesquare) & (Bitboards[opposite] & Bitboards[j])) {
          captured = j;
        }
      }
      notation |= (1 << 16);
      notation |= (captured << 17);
      moves[depth][movecount] = notation;
      movecount++;
      ourcaptures ^= (1ULL << capturesquare);
    }
    if (!capturesonly) {
      ourmoves = ourmask & (~preoccupied);
      int movenumber = __builtin_popcountll(ourmoves);
      for (int j = 0; j < movenumber; j++) {
        int movesquare = __builtin_popcountll((ourmoves & -ourmoves) - 1);
        int notation = rooksquare | (movesquare << 6);
        notation |= (color << 12);
        notation |= (5 << 13);
        moves[depth][movecount] = notation;
        movecount++;
        ourmoves ^= (1ULL << movesquare);
      }
    }
    ourrooks ^= (1ULL << rooksquare);
  }
  for (int i = 0; i < queencount; i++) {
    int queensquare = __builtin_popcountll((ourqueens & -ourqueens) - 1);
    U64 pinmask = ~(0ULL);
    if (pinnedpieces & (1ULL << queensquare)) {
      int queenrank = queensquare & 56;
      int queenfile = queensquare & 7;
      if (queenrank == pinrank) {
        pinmask = GetRankAttacks(preoccupied, queensquare);
      } else if (queenfile == pinfile) {
        pinmask = FileAttacks(preoccupied, queensquare);
      } else if ((queenfile - pinfile) * (queenrank - pinrank) > 0) {
        pinmask = DiagAttacks(preoccupied, queensquare);
      } else {
        pinmask = AntiAttacks(preoccupied, queensquare);
      }
    }
    ourmask = (GetRankAttacks(preoccupied, queensquare) |
               FileAttacks(preoccupied, queensquare));
    ourmask |= (DiagAttacks(preoccupied, queensquare) |
                AntiAttacks(preoccupied, queensquare));
    ourmask &= (pinmask & checkmask);
    ourcaptures = ourmask & Bitboards[opposite];
    int capturenumber = __builtin_popcountll(ourcaptures);
    for (int j = 0; j < capturenumber; j++) {
      int capturesquare =
          __builtin_popcountll((ourcaptures & -ourcaptures) - 1);
      int notation = queensquare | (capturesquare << 6);
      notation |= (color << 12);
      notation |= (6 << 13);
      int captured = 0;
      for (int j = 2; j < 7; j++) {
        if ((1ULL << capturesquare) & (Bitboards[opposite] & Bitboards[j])) {
          captured = j;
        }
      }
      notation |= (1 << 16);
      notation |= (captured << 17);
      moves[depth][movecount] = notation;
      movecount++;
      ourcaptures ^= (1ULL << capturesquare);
    }
    if (!capturesonly) {
      ourmoves = ourmask & (~preoccupied);
      int movenumber = __builtin_popcountll(ourmoves);
      for (int j = 0; j < movenumber; j++) {
        int movesquare = __builtin_popcountll((ourmoves & -ourmoves) - 1);
        int notation = queensquare | (movesquare << 6);
        notation |= (color << 12);
        notation |= (6 << 13);
        moves[depth][movecount] = notation;
        movecount++;
        ourmoves ^= (1ULL << movesquare);
      }
    }
    ourqueens ^= (1ULL << queensquare);
  }
  if (position & (1 << (14 + 2 * color))) {
    if (!((opponentattacks | occupied) &
          ((1ULL << (kingsquare + 3)) - (1ULL << kingsquare)))) {
      int notation = kingsquare | ((kingsquare + 2) << 6);
      notation |= (color << 12);
      notation |= (7 << 13);
      notation |= (1 << 23);
      moves[depth][movecount] = notation;
      movecount++;
    }
  }
  if (position & (1 << (15 + 2 * color))) {
    if (!(((opponentattacks) &
           ((1ULL << (kingsquare + 1)) - (1ULL << (kingsquare - 2)))) ||
          (occupied & ((1ULL << kingsquare) - (1ULL << (kingsquare - 3)))))) {
      int notation = kingsquare | ((kingsquare - 2) << 6);
      notation |= (color << 12);
      notation |= (7 << 13);
      notation |= (1 << 23);
      moves[depth][movecount] = notation;
      movecount++;
    }
  }
  return movecount;
}
U64 Board::perft(int depth, int initialdepth, int color) {
  int movcount = generatemoves(color, 0, depth);
  U64 ans = 0;
  if (depth > 1) {
    for (int i = 0; i < movcount; i++) {
      makemove(moves[depth][i], true);
      if (depth == initialdepth) {
        std::cout << algebraic(moves[depth][i]) << ": ";
      }
      ans += perft(depth - 1, initialdepth, color ^ 1);
      unmakemove(moves[depth][i]);
    }
    if (depth == initialdepth - 1) {
      std::cout << ans << " ";
    }
    if (depth == initialdepth) {
      std::cout << "\n" << ans << "\n";
    }
    return ans;
  } else {
    if (initialdepth == 2) {
      std::cout << movcount << " ";
    }
    return movcount;
  }
}
U64 Board::perftnobulk(int depth, int initialdepth, int color) {
  int movcount = generatemoves(color, 0, depth);
  U64 ans = 0;
  for (int i = 0; i < movcount; i++) {
    makemove(moves[depth][i], true);
    if (depth == initialdepth) {
      std::cout << algebraic(moves[depth][i]) << ": ";
    }
    if (depth > 1) {
      ans += perftnobulk(depth - 1, initialdepth, color ^ 1);
    } else {
      ans++;
    }
    unmakemove(moves[depth][i]);
  }
  if (depth == initialdepth - 1) {
    std::cout << ans << " ";
  }
  if (depth == initialdepth) {
    std::cout << "\n" << ans << "\n";
  }
  return ans;
}
void Board::parseFEN(std::string FEN) {
  gamelength = 0;
  last = 0;
  root = 0;
  gamephase[0] = 0;
  gamephase[1] = 0;
  int order[64] = {56, 57, 58, 59, 60, 61, 62, 63, 48, 49, 50, 51, 52,
                   53, 54, 55, 40, 41, 42, 43, 44, 45, 46, 47, 32, 33,
                   34, 35, 36, 37, 38, 39, 24, 25, 26, 27, 28, 29, 30,
                   31, 16, 17, 18, 19, 20, 21, 22, 23, 8,  9,  10, 11,
                   12, 13, 14, 15, 0,  1,  2,  3,  4,  5,  6,  7};
  char file[8] = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'};
  int progress = 0;
  for (int i = 0; i < 8; i++) {
    Bitboards[i] = 0ULL;
  }
  int tracker = 0;
  int castling = 0;
  int color = 0;
  while (FEN[tracker] != ' ') {
    char hm = FEN[tracker];
    if ('0' <= hm && hm <= '9') {
      int repeat = (int)hm - 48;
      progress += repeat;
    }
    if ('A' <= hm && hm <= 'Z') {
      Bitboards[0] |= (1ULL << order[progress]);
      if (hm == 'P') {
        Bitboards[2] |= (1ULL << order[progress]);
      }
      if (hm == 'N') {
        Bitboards[3] |= (1ULL << order[progress]);
        gamephase[0] += 1;
      }
      if (hm == 'B') {
        Bitboards[4] |= (1ULL << order[progress]);
        gamephase[0] += 1;
      }
      if (hm == 'R') {
        Bitboards[5] |= (1ULL << order[progress]);
        gamephase[0] += 2;
      }
      if (hm == 'Q') {
        Bitboards[6] |= (1ULL << order[progress]);
        gamephase[0] += 4;
      }
      if (hm == 'K') {
        Bitboards[7] |= (1ULL << order[progress]);
      }
      progress++;
    }
    if ('a' <= hm && hm <= 'z') {
      Bitboards[1] |= (1ULL << order[progress]);
      if (hm == 'p') {
        Bitboards[2] |= (1ULL << order[progress]);
      }
      if (hm == 'n') {
        Bitboards[3] |= (1ULL << order[progress]);
        gamephase[1] += 1;
      }
      if (hm == 'b') {
        Bitboards[4] |= (1ULL << order[progress]);
        gamephase[1] += 1;
      }
      if (hm == 'r') {
        Bitboards[5] |= (1ULL << order[progress]);
        gamephase[1] += 2;
      }
      if (hm == 'q') {
        Bitboards[6] |= (1ULL << order[progress]);
        gamephase[1] += 4;
      }
      if (hm == 'k') {
        Bitboards[7] |= (1ULL << order[progress]);
      }
      progress++;
    }
    tracker++;
  }
  while (FEN[tracker] == ' ') {
    tracker++;
  }
  if (FEN[tracker] == 'b') {
    color = 1;
  }
  position = color;
  tracker += 2;
  while (FEN[tracker] != ' ') {
    char hm = FEN[tracker];
    if (hm == 'K') {
      castling |= 1;
    }
    if (hm == 'Q') {
      castling |= 2;
    }
    if (hm == 'k') {
      castling |= 4;
    }
    if (hm == 'q') {
      castling |= 8;
    }
    tracker++;
  }
  tracker++;
  position |= (castling << 14);
  if (FEN[tracker] == '-') {
    tracker += 2;
  } else {
    int epfile = 0;
    for (int j = 0; j < 8; j++) {
      if (file[j] == FEN[tracker]) {
        epfile = j;
      }
    }
    tracker++;
    int eprank = (int)(FEN[tracker]) - 49;
    int epsquare = 8 * eprank + epfile;
    position |= (epsquare << 8);
    tracker += 2;
  }
  int halfmove = (int)(FEN[tracker]) - 48;
  tracker++;
  if (FEN[tracker] != ' ') {
    halfmove = 10 * halfmove + (int)(FEN[tracker]) - 48;
  }
  position |= (halfmove << 1);
  zobristhash = scratchzobrist();
  zobrist[0] = zobristhash;
  history[0] = position;
}
std::string Board::getFEN() {
  int order[64] = {56, 57, 58, 59, 60, 61, 62, 63, 48, 49, 50, 51, 52,
                   53, 54, 55, 40, 41, 42, 43, 44, 45, 46, 47, 32, 33,
                   34, 35, 36, 37, 38, 39, 24, 25, 26, 27, 28, 29, 30,
                   31, 16, 17, 18, 19, 20, 21, 22, 23, 8,  9,  10, 11,
                   12, 13, 14, 15, 0,  1,  2,  3,  4,  5,  6,  7};
  std::string FEN = "";
  int empt = 0;
  char convert[2][6] = {{'P', 'N', 'B', 'R', 'Q', 'K'},
                        {'p', 'n', 'b', 'r', 'q', 'k'}};
  int color;
  int piece;
  for (int i = 0; i < 64; i++) {
    color = -1;
    for (int j = 0; j < 2; j++) {
      if (Bitboards[j] & (1ULL << order[i])) {
        color = j;
      }
    }
    if (color >= 0) {
      if (empt > 0) {
        FEN = FEN + (char)(empt + 48);
        empt = 0;
      }
      for (int j = 0; j < 6; j++) {
        if (Bitboards[j + 2] & (1ULL << order[i])) {
          piece = j;
        }
      }
      FEN = FEN + (convert[color][piece]);
    } else {
      empt++;
      if ((i & 7) == 7) {
        FEN = FEN + (char)(empt + 48);
        empt = 0;
      }
    }
    if (((i & 7) == 7) && (i < 63)) {
      FEN = FEN + '/';
    }
  }
  FEN = FEN + ' ';
  if (position & 1) {
    FEN = FEN + "b ";
  } else {
    FEN = FEN + "w ";
  }
  int castling = (position >> 14) & 63;
  if (castling == 0) {
    FEN = FEN + "-";
  } else {
    for (int i = 0; i < 4; i++) {
      if ((castling >> i) & 1) {
        FEN = FEN + "KQkq"[i];
      }
    }
  }
  FEN = FEN + " ";
  int epsquare = (position >> 8) & 63;
  if (epsquare) {
    FEN = FEN + coordinate(epsquare) + " ";
  } else {
    FEN = FEN + "- ";
  }
  int halfmove = (position >> 1) & 127;
  std::string bruh = "";
  do {
    bruh = bruh + (char)(halfmove % 10 + 48);
    halfmove /= 10;
  } while (halfmove > 0);
  reverse(bruh.begin(), bruh.end());
  FEN = FEN + bruh + " 1";
  return FEN;
}
bool Board::see_exceeds(int move, int color, int threshold) {
  int see_values[6] = {10, 30, 30, 50, 90, 20000};
  int target = (move >> 6) & 63;
  int victim = (move >> 17) & 7;
  int attacker = (move >> 13) & 7;
  int value = (victim > 0) ? see_values[victim - 2] - threshold : -threshold;
  if (value < 0) {
    return false;
  }
  if (value - see_values[attacker - 2] >= 0) {
    return true;
  }
  int pieces[2][6] = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}};
  U64 occupied = Bitboards[0] | Bitboards[1];
  U64 to = (1ULL << target);
  U64 from = (1ULL << (move & 63));
  U64 us = Bitboards[color];
  U64 enemy = Bitboards[color ^ 1];
  U64 knights = KnightAttacks[target] & Bitboards[3];
  U64 kings = KingAttacks[target] & Bitboards[7];
  pieces[0][0] =
      __builtin_popcountll((PawnAttacks[color][target] & Bitboards[2] & enemy));
  pieces[1][0] = __builtin_popcountll(
      (PawnAttacks[color ^ 1][target] & Bitboards[2] & us));
  pieces[0][1] = __builtin_popcountll(knights & enemy);
  pieces[1][1] = __builtin_popcountll(knights & us);
  pieces[0][5] = __builtin_popcountll(kings & enemy);
  pieces[1][5] = __builtin_popcountll(kings & us);
  occupied ^= (enemy & Bitboards[5]);
  occupied |= to;
  occupied &= (~from);
  pieces[0][3] = __builtin_popcountll(
      (FileAttacks(occupied, target) | GetRankAttacks(occupied, target)) &
      Bitboards[5] & enemy);
  occupied ^= (Bitboards[5]);
  occupied |= to;
  occupied &= (~from);
  pieces[1][3] = __builtin_popcountll(
      (FileAttacks(occupied, target) | GetRankAttacks(occupied, target)) &
      Bitboards[5] & us);
  occupied ^= (us & (Bitboards[5] | Bitboards[2] | Bitboards[4]));
  occupied |= to;
  occupied &= (~from);
  pieces[1][2] = __builtin_popcountll(
      (DiagAttacks(occupied, target) | AntiAttacks(occupied, target)) &
      Bitboards[4] & us);
  occupied ^= (Bitboards[4] | Bitboards[2]);
  occupied |= to;
  occupied &= (~from);
  pieces[0][2] = __builtin_popcountll(
      (DiagAttacks(occupied, target) | AntiAttacks(occupied, target)) &
      Bitboards[4] & enemy);
  occupied ^= (enemy & (Bitboards[5] | Bitboards[6]));
  occupied |= to;
  occupied &= (~from);
  pieces[0][4] += __builtin_popcountll(
      (FileAttacks(occupied, target) | GetRankAttacks(occupied, target)) &
      Bitboards[6] & enemy);
  occupied ^= (enemy & (Bitboards[2] | Bitboards[4] | Bitboards[5]));
  occupied |= to;
  occupied &= (~from);
  pieces[0][4] += __builtin_popcountll(
      (DiagAttacks(occupied, target) | AntiAttacks(occupied, target)) &
      Bitboards[6] & enemy);
  occupied ^= (Bitboards[2] | Bitboards[4] | Bitboards[6]);
  occupied |= to;
  occupied &= (~from);
  pieces[1][4] += __builtin_popcountll(
      (DiagAttacks(occupied, target) | AntiAttacks(occupied, target)) &
      Bitboards[6] & us);
  occupied ^= (us & (Bitboards[2] | Bitboards[4] | Bitboards[5]));
  occupied |= to;
  occupied &= (~from);
  pieces[1][4] += __builtin_popcountll(
      (FileAttacks(occupied, target) | GetRankAttacks(occupied, target)) &
      Bitboards[6] & us);
  if (attacker > 2) {
    pieces[1][attacker - 2]--;
  }
  int next[2] = {0, 0};
  int previous[2] = {0, attacker - 2};
  int i = 0;
  while (true) {
    while (pieces[i][next[i]] == 0 && next[i] < 6) {
      next[i]++;
    }
    if (next[i] > 5) {
      return (value >= 0);
    }
    value += (2 * i - 1) * see_values[previous[i ^ 1]];
    if ((2 * i - 1) * (value + (1 - 2 * i) * see_values[next[i]]) >= 1 - i) {
      return i;
    }
    previous[i] = next[i];
    pieces[i][next[i]]--;
    i ^= 1;
  }
}
