#include <iostream>
#include <algorithm>
#include <time.h>
#include <bit>
#include <random>
#include <string>
#include <chrono>
#include <fstream>
using U64 = uint64_t;
using namespace std;


void printbitboard(U64 bitboard) {
    U64 other = byteswap(bitboard);
    for (int i = 0; i < 64; i++) {
        cout << (other%2);
        other = other >> 1;
        if (i == (i|7)) {
            cout << endl;
        }
    }
}
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
U64 Bitboards[8];
//White, Black, Pawn, Alfil, Ferz, Knight, Rook, King
U64 Files[8] = {FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH};
U64 Ranks[8] = {Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8};
U64 KingAttacks[64];
U64 PawnAttacks[2][64];
U64 AlfilAttacks[64];
U64 FerzAttacks[64];
U64 KnightAttacks[64];
U64 RankMask[64];
U64 FileMask[64];
U64 RankAttacks[512];
U64 hashes[8][64];
U64 colorhash = 0xE344F58E0F3B26E5;
U64 zobrist[1024];
int history[1024];
int gamelength = 0;
int last = 0;
int root = 0;
int moves[64][256];
int movescore[64][256];
int maxdepth = 32;
int killers[32][2];
int position = 0;
int evalm[2] = {0, 0};
int evale[2] = {0, 0};
int nodecount = 0;
int bestmove = 0;
U64 zobristhash = 0ULL;
int movetime = 0;
string proto = "uci";
bool gosent = false;
bool stopsearch = false;
//1 bit color, 7 bits halfmove, 6 bits ep, 4 bits castling KQkq
//6 bits from square, 6 bits to square, 1 bit color, 3 bits piece moved, 1 bit castling, 1 bit double pawn push,
//1 bit en passant, 1 bit promotion, 2 bits promoted piece, 1 bit capture, 3 bits piece captured
//26 bits total for now?
int movecount;
auto start = chrono::steady_clock::now();
int materialm[6] = {87, 103, 144, 331, 564, 20000};
int materiale[6] = {119, 92, 185, 380, 755, 20000};
int pstm[6][64] = {{0,0,0,0,0,0,0,0,-23,-7,-16,-37,-43,-19,-6,-13,-10,9,1,-8,-16,4,12,-18,-17,-12,-9,5,2,-3,-22,-14,
-6,-8,-5,-7,3,-9,-14,-10,1,17,13,20,26,18,4,-4,23,32,27,38,47,35,19,16,0,0,0,0,0,0,0,0},
{-41,-35,-14,-17,-19,-7,-33,-45,-38,-36,-4,-7,-5,1,-32,-37,-22,-15,9,8,11,6,-17,-26,-17,-12,13,26,23,18,-13,-18,
-19,3,15,24,23,14,-4,-20,-25,-8,12,19,17,19,-9,-23,-36,-21,7,5,6,2,-28,-35,-39,-33,-17,-11,-16,-22,-31,-38},
{-39,-23,-26,-22,-18,-21,-27,-34,-29,-15,-9,-14,-19,-5,-21,-32,-31,-13,-2,3,11,6,-3,-22,-23,-17,6,18,15,4,-2,-17,
-16,-8,1,14,43,13,7,-19,-5,-13,-7,22,19,31,9,-11,-26,-14,0,5,6,-2,-7,-25,-32,-20,-14,-9,-6,-19,-17,-37},
{-42,-11,-3,7,4,-5,-16,-37,-28,-10,-5,19,13,0,-8,-31,-21,-12,15,7,3,21,-1,-9,-23,-2,8,35,28,19,12,-11,
-16,-7,13,44,39,26,25,2,-20,-10,5,24,29,18,6,-17,-31,-8,-2,7,11,0,-3,-29,-47,-23,-13,-1,2,5,-7,-42},
{-29,-22,-13,-7,-9,-12,-17,-30,-24,-19,-14,1,-2,13,-1,-11,-17,-13,-11,-4,-16,1,9,-3,-15,-15,-10,-6,-9,5,-1,4,
-8,-10,-5,-3,-12,0,3,-7,-11,-7,-8,2,-5,8,-7,1,-2,6,11,14,9,17,5,8,3,8,9,16,15,13,9,10},
{-13,-3,5,-2,1,-9,-5,-16,3,12,19,8,4,17,13,-1,-5,0,4,7,2,5,-2,-8,-12,-9,-15,-19,-20,-17,-13,-11,
-18,-16,-23,-27,-31,-22,-18,-17,-17,-17,-15,-20,-26,-24,-21,-18,-19,-17,-18,-22,-24,-19,-23,-20,-18,-20,-19,-25,-22,-16,-18,-14}};
int pste[6][64] = {{0,0,0,0,0,0,0,0,-23,-7,-16,-37,-43,-19,-6,-13,-10,9,1,-8,-16,4,12,-18,-17,-12,-9,5,2,-3,-22,-14,
-6,-8,-5,-7,3,-9,-14,-10,1,17,13,20,26,18,4,-4,23,32,27,38,47,35,19,16,0,0,0,0,0,0,0,0},
{-41,-35,-14,-17,-19,-7,-33,-45,-38,-36,-4,-7,-5,1,-32,-37,-22,-15,9,8,11,6,-17,-26,-17,-12,13,26,23,18,-13,-18,
-19,3,15,24,23,14,-4,-20,-25,-8,12,19,17,19,-9,-23,-36,-21,7,5,6,2,-28,-35,-39,-33,-17,-11,-16,-22,-31,-38},
{-39,-23,-26,-22,-18,-21,-27,-34,-29,-15,-9,-14,-19,-5,-21,-32,-31,-13,-2,3,11,6,-3,-22,-23,-17,6,18,15,4,-2,-17,
-16,-8,1,14,43,13,7,-19,-5,-13,-7,22,19,31,9,-11,-26,-14,0,5,6,-2,-7,-25,-32,-20,-14,-9,-6,-19,-17,-37},
{-42,-11,-3,7,4,-5,-16,-37,-28,-10,-5,19,13,0,-8,-31,-21,-12,15,7,3,21,-1,-9,-23,-2,8,35,28,19,12,-11,
-16,-7,13,44,39,26,25,2,-20,-10,5,24,29,18,6,-17,-31,-8,-2,7,11,0,-3,-29,-47,-23,-13,-1,2,5,-7,-42},
{-29,-22,-13,-7,-9,-12,-17,-30,-24,-19,-14,1,-2,13,-1,-11,-17,-13,-11,-4,-16,1,9,-3,-15,-15,-10,-6,-9,5,-1,4,
-8,-10,-5,-3,-12,0,3,-7,-11,-7,-8,2,-5,8,-7,1,-2,6,11,14,9,17,5,8,3,8,9,16,15,13,9,10},
{-33,-31,-27,-22,-24,-29,-34,-42,-23,-19,-6,1,-4,-9,-17,-30,-13,-5,2,7,4,0,-7,-17,-8,-2,11,14,15,9,4,-9,
-6,3,9,19,17,12,5,-4,-7,-1,4,11,8,10,1,-7,-18,-10,-7,0,-2,4,-8,-23,-29,-24,-18,-11,-16,-13,-20,-31}};
int lmr_reductions[32][256];
int historytable[2][6][64];
int startpiece[16] = {4, 3, 1, 5, 2, 1, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0};
int phase[6] = {0, 1, 2, 4, 6, 0};
int gamephase[2] = {0, 0};
int block[17] = {24, 24, 24, 24, 23, 23, 23, 22, 22, 21, 20, 18, 16, 13, 10, 8, 6};
ofstream bookoutput;
struct TTentry {
    U64 key;
    int score;
    int depth;
    int age;
    int nodetype;
    int hashmove;
};
int TTsize = 1349651;
TTentry TT[1349651];
U64 shift_w(U64 bitboard) {
    return (bitboard & ~FileA) >> 1;
}
U64 shift_n(U64 bitboard) {
    return bitboard << 8;
}
U64 shift_s(U64 bitboard) {
    return bitboard >> 8;
}
U64 shift_e(U64 bitboard) {
    return (bitboard & ~FileH) << 1;
}
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
        FerzAttacks[i] = shift_n(shift_w(square)|shift_e(square)) | shift_s(shift_w(square)|shift_e(square));
        U64 alfilattack = ((square & ~FileA & ~FileB) << 14);
        alfilattack |= ((square & ~FileA & ~FileB) >> 18);
        alfilattack |= ((square & ~FileG & ~FileH) << 18);
        alfilattack |= ((square & ~FileG & ~FileH) >> 14);
        AlfilAttacks[i] = alfilattack;
    }
}
void initializemasks() {
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            RankMask[8*i+j] = Ranks[i];
            FileMask[8*i+j] = Files[j];
        }
    }
}
void initializerankattacks() {
    for (U64 i = 0ULL; i < 0x000000000040; i++) {
        U64 occupied = i << 1;
        for (int j = 0; j < 8; j++) {
            U64 attacks = 0ULL;
            if (j > 0) {
                int k = j-1;
                while (k >= 0) {
                    attacks |= (1ULL << k);
                    if ((1ULL << k) & occupied) {
                        k = 0;
                    }
                    k--;
                }
            }
            if (j < 7) {
                int k = j+1;
                while (k <= 7) {
                    attacks |= (1ULL << k);
                    if ((1ULL << k) & occupied) {
                        k = 7;
                    }
                    k++;
                }
            }
            RankAttacks[8*i+j] = attacks;
        }
    }
}
U64 FileAttacks(U64 occupied, int square) {
    U64 forwards = occupied & FileMask[square];
    U64 backwards = byteswap(forwards);
    forwards = forwards - 2*(1ULL << square);
    backwards = backwards - 2*(1ULL << (56^square));
    backwards = byteswap(backwards);
    return (forwards^backwards) & FileMask[square];
}
U64 GetRankAttacks(U64 occupied, int square) {
    int row = square & 56;
    int file = square & 7;
    int relevant = (occupied >> (row+1)) & 63;
    return (RankAttacks[8*relevant+file] << row);
}
void initializezobrist() {
    mt19937_64 mt(20346892);
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 64; j++) {
            hashes[i][j] = mt();
        }
    }
}
U64 scratchzobrist() {
    U64 scratch = 0ULL;
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 64; j++) {
            if (Bitboards[i]&(1ULL << j)) {
                scratch^=hashes[i][j];
            }
        }
    }
    if (position & 1) {
        scratch^=colorhash;
    }
    return scratch;
}
void initializett() {
    for (int i = 0; i < TTsize; i++) {
        TT[i].key = (U64)i+1ULL;
        TT[i].score = 0;
        TT[i].depth = 0;
        TT[i].age = 0;
        TT[i].nodetype = 0;
        TT[i].hashmove = 0;
    }
}
void updatett(int index, int depth, int score, int nodetype, int hashmove) {
    if (index < TTsize) {
        TT[index].key = zobristhash;
        TT[index].depth = depth;
        TT[index].age = gamelength;
        TT[index].hashmove = hashmove;
        TT[index].nodetype = nodetype;
        TT[index].score = score;
    }
}
void resethistory() {
    for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 64; j++) {
            historytable[0][i][j] /= 2;
            historytable[1][i][j] /= 2;
        }
    }
}
void initializeboard() {
    Bitboards[0] = Rank1 | Rank2;
    Bitboards[1] = Rank7 | Rank8;
    Bitboards[2] = Rank2 | Rank7;
    Bitboards[3] = (Rank1 | Rank8) & (FileC | FileF);
    Bitboards[4] = (Rank1 | Rank8) & FileE;
    Bitboards[5] = (Rank1 | Rank8) & (FileB | FileG);
    Bitboards[6] = (Rank1 | Rank8) & (FileA | FileH);
    Bitboards[7] = (Rank1 | Rank8) & FileD;
    position = 0;
    history[0] = position;
    int startmatm = (8*materialm[0]+2*(materialm[1]+materialm[3]+materialm[4])+materialm[2]);
    int startmate = (8*materiale[0]+2*(materiale[1]+materiale[3]+materiale[4])+materiale[2]);
    int startpstm = 0;
    int startpste = 0;
    for (int i = 0; i < 16; i++) {
        startpstm+=pstm[startpiece[i]][i];
        startpste+=pste[startpiece[i]][i];
    }
    for (int i = 0; i < 32; i++) {
        killers[i][0] = 0;
        killers[i][1] = 0;
    }
    evalm[0] = startmatm+startpstm;
    evalm[1] = startmatm+startpstm;
    evale[0] = startmate+startpste;
    evale[1] = startmate+startpste;
    gamephase[0] = 24;
    gamephase[1] = 24;
    gamelength = 0;
    zobrist[0] = scratchzobrist();
}
void initializelmr() {
    for (int i = 0; i < maxdepth; i++) {
        for (int j = 0; j < 256; j++) {
            lmr_reductions[i][j] = (i == 0 || j == 0) ? 0 : floor(1+log(i)*log(j)/4.00);
        }
    }
}
int repetitions() {
    int repeats = 0;
    for (int i = gamelength-2; i>=last; i-=2) {
        if (zobrist[i] == zobrist[gamelength]) {
            repeats++;
            if (i >= root) {
                repeats++;
            }
        }
    }
    return repeats;
}
U64 checkers(int color) {
    int kingsquare = popcount((Bitboards[color] & Bitboards[7])-1);
    int opposite = color^1;
    U64 attacks = 0ULL;
    U64 occupied = Bitboards[0] | Bitboards[1];
    attacks |= (KnightAttacks[kingsquare]&Bitboards[5]);
    attacks |= (PawnAttacks[color][kingsquare]&Bitboards[2]);
    attacks |= (AlfilAttacks[kingsquare]&Bitboards[3]);
    attacks |= (FerzAttacks[kingsquare]&Bitboards[4]);
    attacks |= (GetRankAttacks(occupied, kingsquare)&Bitboards[6]);
    attacks |= (FileAttacks(occupied, kingsquare)&Bitboards[6]);
    attacks &= Bitboards[opposite];
    return attacks;
}
void makenullmove() {
    gamelength++;
    int halfmove = (position >> 1)&127;
    zobristhash^=colorhash;
    position^=(halfmove << 1);
    halfmove++;
    position^=(halfmove << 1);
    position^=1;
    zobrist[gamelength] = zobristhash;
    history[gamelength] = position;
}
void unmakenullmove() {
    gamelength--;
    position = history[gamelength];
    zobristhash = zobrist[gamelength];
}
void makemove(int notation, bool reversible) {
    //6 bits from square, 6 bits to square, 1 bit color, 3 bits piece moved, 1 bit capture, 3 bits piece captured, 1 bit promotion,
    //2 bits promoted piece, 1 bit castling, 1 bit double pawn push, 1 bit en passant,
    //26 bits total

    //1 bit color, 7 bits halfmove, 6 bits ep, 4 bits castling
    gamelength++;
    if (!reversible) {
        root = gamelength;
    }
    int from = notation & 63;
    int to = (notation >> 6) & 63;
    int color = (notation >> 12) & 1;
    int piece = (notation >> 13) & 7;
    Bitboards[color]^=((1ULL << from) | (1ULL << to));
    Bitboards[piece]^=((1ULL << from) | (1ULL << to));
    evalm[color] += pstm[piece-2][(56*color)^to];
    evalm[color] -= pstm[piece-2][(56*color)^from];
    evale[color] += pste[piece-2][(56*color)^to];
    evale[color] -= pste[piece-2][(56*color)^from];
    zobristhash^=(hashes[color][from]^hashes[color][to]);
    zobristhash^=(hashes[piece][from]^hashes[piece][to]);
    int captured = (notation >> 17) & 7;
    int promoted = (notation >> 21) & 3;
    int halfmove = (position >> 1) & 127;
    position^=(halfmove << 1);
    halfmove++;
    position&=0x0003C0FF;
    if (piece==2) {
        halfmove = 0;
        if (!reversible) {
            last = gamelength;
        }
    }
    if (notation & (1 << 16)) {
        Bitboards[color^1]^=(1ULL << to);
        Bitboards[captured]^=(1ULL << to);
        zobristhash^=(hashes[color^1][to]^hashes[captured][to]);
        evalm[color^1]-=materialm[captured-2];
        evale[color^1]-=materiale[captured-2];
        evalm[color^1]-=pstm[captured-2][(56*(color^1))^to];
        evale[color^1]-=pste[captured-2][(56*(color^1))^to];
        gamephase[color^1]-=phase[captured-2];
        halfmove = 0;
        if (!reversible) {
            last = gamelength;
        }
    }
    if (notation & (1 << 20)) {
        Bitboards[2]^=(1ULL << to);
        Bitboards[promoted+3]^=(1ULL << to);
        zobristhash^=(hashes[2][to]^hashes[promoted+3][to]);
        evalm[color]-=(materialm[0]+pstm[0][(56*color)^from]);
        evalm[color]+=(materialm[promoted+1]+pstm[promoted+1][(56*color)^from]);
        evale[color]-=(materiale[0]+pste[0][(56*color)^from]);
        evale[color]+=(materiale[promoted+1]+pste[promoted+1][(56*color)^to]);
        gamephase[color]+=phase[promoted+1];
    }
    else if (notation & (1 << 24)) {
        position^=((from+to) << 7);
    }
    position^=1;
    position^=(halfmove << 1);
    zobristhash^=colorhash;
    history[gamelength] = position;
    zobrist[gamelength] = zobristhash;
    nodecount++;
}
void unmakemove(int notation) {
    gamelength--;
    position = history[gamelength];
    zobristhash = zobrist[gamelength];
    int from = notation & 63;
    int to = (notation >> 6) & 63;
    int color = (notation >> 12) & 1;
    int piece = (notation >> 13) & 7;
    Bitboards[color]^=((1ULL << from) | (1ULL << to));
    Bitboards[piece]^=((1ULL << from) | (1ULL << to));
    evalm[color] += pstm[piece-2][(56*color)^from];
    evalm[color] -= pstm[piece-2][(56*color)^to];
    evale[color] += pste[piece-2][(56*color)^from];
    evale[color] -= pste[piece-2][(56*color)^to];
    int captured = (notation >> 17) & 7;
    int promoted = (notation >> 21) & 3;
    if (notation & (1 << 16)) {
        Bitboards[color^1]^=(1ULL << to);
        Bitboards[captured]^=(1ULL << to);
        evalm[color^1]+=materialm[captured-2];
        evale[color^1]+=materiale[captured-2];
        evalm[color^1]+=pstm[captured-2][(56*(color^1))^to];
        evale[color^1]+=pste[captured-2][(56*(color^1))^to];
        gamephase[color^1]+=phase[captured-2];
    }
    if (notation & (1 << 20)) {
        Bitboards[2]^=(1ULL << to);
        Bitboards[promoted+3]^=(1ULL << to);
        evalm[color]+=(materialm[0]+pstm[0][(56*color)^from]);
        evalm[color]-=(materialm[promoted+1]+pstm[promoted+1][(56*color)^from]);
        evale[color]+=(materiale[0]+pste[0][(56*color)^from]);
        evale[color]-=(materiale[promoted+1]+pste[promoted+1][(56*color)^to]);
        gamephase[color]-=phase[promoted+1];
    }
}
int generatemoves(int color, bool capturesonly, int depth) {
    movecount = 0;
    int kingsquare = popcount((Bitboards[color] & Bitboards[7])-1);
    int pinrank = kingsquare&56;
    int pinfile = kingsquare&7;
    int opposite = color^1;
    U64 opponentattacks = 0ULL;
    U64 pinnedpieces = 0ULL;
    U64 checkmask = 0ULL;
    U64 preoccupied = Bitboards[0] | Bitboards[1];
    U64 kingRank = GetRankAttacks(preoccupied, kingsquare);
    U64 kingFile = FileAttacks(preoccupied, kingsquare);
    U64 occupied = preoccupied^(1ULL << kingsquare);
    U64 opponentpawns = Bitboards[opposite]&Bitboards[2];
    U64 opponentalfils = Bitboards[opposite]&Bitboards[3];
    U64 opponentferzes = Bitboards[opposite]&Bitboards[4];
    U64 opponentknights = Bitboards[opposite]&Bitboards[5];
    U64 opponentrooks = Bitboards[opposite]&Bitboards[6];
    int pawncount = popcount(opponentpawns);
    int alfilcount = popcount(opponentalfils);
    int ferzcount = popcount(opponentferzes);
    int knightcount = popcount(opponentknights);
    int rookcount = popcount(opponentrooks);
    U64 ourcaptures = 0ULL;
    U64 ourmoves = 0ULL;
    U64 ourmask = 0ULL;
    for (int i = 0; i < pawncount; i++) {
        int pawnsquare = popcount((opponentpawns & -opponentpawns)-1);
        opponentattacks |= PawnAttacks[opposite][pawnsquare];
        opponentpawns^=(1ULL << pawnsquare);
    }
    for (int i = 0; i < alfilcount; i++) {
        int alfilsquare = popcount((opponentalfils & -opponentalfils)-1);
        opponentattacks |= AlfilAttacks[alfilsquare];
        opponentalfils^=(1ULL << alfilsquare);
    }
    for (int i = 0; i < ferzcount; i++) {
        int ferzsquare = popcount((opponentferzes & -opponentferzes)-1);
        opponentattacks |= FerzAttacks[ferzsquare];
        opponentferzes^=(1ULL << ferzsquare);
    }
    for (int i = 0; i < knightcount; i++) {
        int knightsquare = popcount((opponentknights & -opponentknights)-1);
        opponentattacks |= KnightAttacks[knightsquare];
        opponentknights^=(1ULL << knightsquare);
    }
    for (int i = 0; i < rookcount; i++) {
        int rooksquare = popcount((opponentrooks & -opponentrooks)-1);
        U64 r = GetRankAttacks(occupied, rooksquare);
        U64 file = FileAttacks(occupied, rooksquare);
        if (!(r&(1ULL << kingsquare))) {
            pinnedpieces |= (r & kingRank);
        }
        else {
            checkmask |= (GetRankAttacks(preoccupied, rooksquare) & kingRank);
        }
        if (!(file&(1ULL << kingsquare))) {
            pinnedpieces |= (file & kingFile);
        }
        else {
            checkmask |= (FileAttacks(preoccupied, rooksquare) & kingFile);
        }
        opponentattacks |= (r | file);
        opponentrooks^=(1ULL << rooksquare);
    }
    int opponentking = popcount((Bitboards[opposite]&Bitboards[7])-1);
    opponentattacks |= KingAttacks[opponentking];
    ourcaptures = KingAttacks[kingsquare]&((~opponentattacks)&Bitboards[opposite]);
    int capturenumber = popcount(ourcaptures);
    int movenumber;
    for (int i = 0; i < capturenumber; i++) {
        int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
        int notation = kingsquare | (capturesquare << 6);
        notation |= (color << 12);
        notation |= (7 << 13);
        int captured = 0;
        for (int j = 2; j < 7; j++) {
            if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                captured = j;
            }
        }
        notation |= (1 << 16);
        notation |= (captured << 17);
        moves[depth][movecount] = notation;
        movescore[depth][movecount] = 3000+10000*captured+historytable[color][5][capturesquare];
        movecount++;
        ourcaptures^=(1ULL << capturesquare);
    }
    if (!capturesonly) {
        ourmoves = KingAttacks[kingsquare]&((~opponentattacks)&(~preoccupied));
        movenumber = popcount(ourmoves);
        for (int i = 0; i < movenumber; i++) {
            int movesquare = popcount((ourmoves & -ourmoves)-1);
            int notation = kingsquare | (movesquare << 6);
            notation |= (color << 12);
            notation |= (7 << 13);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = historytable[color][5][movesquare];
            movecount++;
            ourmoves^=(1ULL << movesquare);
        }
    }
    U64 checks = checkers(color);
    if (popcount(checks) > 1) {
        return movecount;
    }
    else if (checks) {
        checkmask |= checks;
    }
    else {
        checkmask = ~(0ULL);
    }
    U64 ourpawns = Bitboards[color]&Bitboards[2];
    U64 ouralfils = Bitboards[color]&Bitboards[3];
    U64 ourferzes = Bitboards[color]&Bitboards[4];
    U64 ourknights = Bitboards[color]&Bitboards[5];
    U64 ourrooks = Bitboards[color]&Bitboards[6];
    pawncount = popcount(ourpawns);
    alfilcount = popcount(ouralfils);
    ferzcount = popcount(ourferzes);
    knightcount = popcount(ourknights);
    rookcount = popcount(ourrooks);
    for (int i = 0; i < pawncount; i++) {
        int pawnsquare = popcount((ourpawns & -ourpawns)-1);
        if ((pinnedpieces&(1ULL << pawnsquare)) && ((pawnsquare&56) == pinrank)) {
            ourpawns^=(1ULL << pawnsquare);
            continue;
        }
        else if ((pinnedpieces&(1ULL << pawnsquare)) && capturesonly) {
            ourpawns^=(1ULL << pawnsquare);
            continue;
        }
        int capturenumber = 0;
        if ((pinnedpieces&(1ULL << pawnsquare)) == 0ULL) {
            ourcaptures = PawnAttacks[color][pawnsquare]&Bitboards[opposite];
            ourcaptures &= checkmask;
            capturenumber = popcount(ourcaptures);
        }
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = pawnsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (2 << 13);
            int captured = 0;
            for (int j = 2; j < 7; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            if (((color==0)&&(capturesquare&56)==56)||((color==1)&&(capturesquare&56)==0)) {
                moves[depth][movecount]=notation|(3 << 20);
                movescore[depth][movecount] = (2+captured)*10000+historytable[color][0][capturesquare];
                movecount++;
            }
            else {
                moves[depth][movecount] = notation;
                movescore[depth][movecount] = 8000+captured*10000+historytable[color][0][capturesquare];
                movecount++;
            }
            ourcaptures^=(1ULL << capturesquare);
        }
        if (!capturesonly) {
            ourmoves = (1ULL << (pawnsquare+8*(1-2*color)))&(~preoccupied);
            ourmoves &= checkmask;
            int movenumber = popcount(ourmoves);
            for (int j = 0; j < movenumber; j++) {
                int movesquare = popcount((ourmoves & -ourmoves)-1);
                int notation = pawnsquare | (movesquare << 6);
                notation |= (color << 12);
                notation |= (2 << 13);
                if (((color==0)&&(movesquare&56)==56)||((color==1)&&(movesquare&56)==0)) {
                    moves[depth][movecount]=notation|(3 << 20);
                    movescore[depth][movecount] = 60000+historytable[color][0][movesquare];
                    movecount++;
                }
                else {
                    moves[depth][movecount]=notation;
                    movescore[depth][movecount] = historytable[color][0][movesquare];
                    movecount++;
                }
                ourmoves^=(1ULL << movesquare);
            }
        }
        ourpawns^=(1ULL << pawnsquare);
    }
    for (int i = 0; i < alfilcount; i++) {
        int alfilsquare = popcount((ouralfils & -ouralfils)-1);
        if (pinnedpieces&(1ULL << alfilsquare)) {
            ouralfils^=(1ULL << alfilsquare);
            continue;
        }
        ourmask = AlfilAttacks[alfilsquare];
        ourmask &= checkmask;
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = alfilsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (3 << 13);
            int captured = 0;
            for (int j = 2; j < 7; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 7000+captured*10000+historytable[color][1][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        if (!capturesonly) {
            ourmoves = ourmask&(~preoccupied);
            int movenumber = popcount(ourmoves);
            for (int j = 0; j < movenumber; j++) {
                int movesquare = popcount((ourmoves & -ourmoves)-1);
                int notation = alfilsquare | (movesquare << 6);
                notation |= (color << 12);
                notation |= (3 << 13);
                moves[depth][movecount]=notation;
                movescore[depth][movecount] = historytable[color][1][movesquare];
                movecount++;
                ourmoves^=(1ULL << movesquare);
            }
        }
        ouralfils^=(1ULL << alfilsquare);
    }
    for (int i = 0; i < ferzcount; i++) {
        int ferzsquare = popcount((ourferzes & -ourferzes)-1);
        if (pinnedpieces&(1ULL << ferzsquare)) {
            ourferzes^=(1ULL << ferzsquare);
            continue;
        }
        ourmask = FerzAttacks[ferzsquare];
        ourmask &= checkmask;
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = ferzsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (4 << 13);
            int captured = 0;
            for (int j = 2; j < 7; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 6000+captured*10000+historytable[color][2][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        if (!capturesonly) {
            ourmoves = ourmask&(~preoccupied);
            int movenumber = popcount(ourmoves);
            for (int j = 0; j < movenumber; j++) {
                int movesquare = popcount((ourmoves & -ourmoves)-1);
                int notation = ferzsquare | (movesquare << 6);
                notation |= (color << 12);
                notation |= (4 << 13);
                moves[depth][movecount]=notation;
                movescore[depth][movecount] = historytable[color][2][movesquare];
                movecount++;
                ourmoves^=(1ULL << movesquare);
            }
        }
        ourferzes^=(1ULL << ferzsquare);
    }
    for (int i = 0; i < knightcount; i++) {
        int knightsquare = popcount((ourknights & -ourknights)-1);
        if (pinnedpieces&(1ULL << knightsquare)) {
            ourknights^=(1ULL << knightsquare);
            continue;
        }
        ourmask = KnightAttacks[knightsquare];
        ourmask &= checkmask;
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = knightsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (5 << 13);
            int captured = 0;
            for (int j = 2; j < 7; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 5000+captured*10000+historytable[color][3][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        if (!capturesonly) {
            ourmoves = ourmask&(~preoccupied);
            int movenumber = popcount(ourmoves);
            for (int j = 0; j < movenumber; j++) {
                int movesquare = popcount((ourmoves & -ourmoves)-1);
                int notation = knightsquare | (movesquare << 6);
                notation |= (color << 12);
                notation |= (5 << 13);
                moves[depth][movecount]=notation;
                movescore[depth][movecount] = historytable[color][3][movesquare];
                movecount++;
                ourmoves^=(1ULL << movesquare);
            }
        }
        ourknights^=(1ULL << knightsquare);
    }
    for (int i = 0; i < rookcount; i++) {
        int rooksquare = popcount((ourrooks & -ourrooks)-1);
        ourmask = (GetRankAttacks(preoccupied, rooksquare)|FileAttacks(preoccupied, rooksquare));
        U64 pinmask = ~(0ULL);
        if (pinnedpieces&(1ULL << rooksquare)) {
            int rookrank = rooksquare&56;
            if (rookrank == pinrank) {
                pinmask = GetRankAttacks(preoccupied, rooksquare);
            }
            else {
                pinmask = FileAttacks(preoccupied, rooksquare);
            }
        }
        ourmask &= (pinmask&checkmask);
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = rooksquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (6 << 13);
            int captured = 0;
            for (int j = 2; j < 7; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 4000+captured*10000+historytable[color][4][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        if (!capturesonly) {
            ourmoves = ourmask&(~preoccupied);
            int movenumber = popcount(ourmoves);
            for (int j = 0; j < movenumber; j++) {
                int movesquare = popcount((ourmoves & -ourmoves)-1);
                int notation = rooksquare | (movesquare << 6);
                notation |= (color << 12);
                notation |= (6 << 13);
                moves[depth][movecount]=notation;
                movescore[depth][movecount] = historytable[color][4][movesquare];
                movecount++;
                ourmoves^=(1ULL << movesquare);
            }
        }
        ourrooks^=(1ULL << rooksquare);
    }
    return movecount;
}
string algebraic(int notation) {
    string convert[64] = { "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6","a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"};
    string header = convert[notation&63]+convert[(notation>>6)&63];
    if (notation&(1 << 20)) {
        header = header+"q";
    }
    return header;
}
U64 perft(int depth, int initialdepth, int color) {
    int movcount = generatemoves(color, 0, depth);
    U64 ans = 0;
    if (depth > 1) {
        for (int i = 0; i < movcount; i++) {
            makemove(moves[depth][i], true);
            if (depth == initialdepth) {
                cout << algebraic(moves[depth][i]);
                cout << ": ";
            }
            ans+=perft(depth-1, initialdepth, color^1);
            unmakemove(moves[depth][i]);
        }
        if (depth == initialdepth-1) {
            cout << ans << " ";
        }
        if (depth == initialdepth) {
            cout << "\n" << ans << "\n";
            auto finish = chrono::steady_clock::now();
            auto diff = chrono::duration_cast<chrono::milliseconds>(finish-start);
            int nps = 1000*(ans/diff.count());
            cout << "nps " << nps << "\n";
        }
        return ans;
    }
    else {
        if (initialdepth == 2) {
            cout << movcount << " ";
        }
        return movcount;
    }
}
U64 perftnobulk(int depth, int initialdepth, int color) {
    int movcount = generatemoves(color, 0, depth);
    U64 ans = 0;
    for (int i = 0; i < movcount; i++) {
        makemove(moves[depth][i], true);
        if (depth == initialdepth) {
            cout << algebraic(moves[depth][i]);
            cout << ": ";
        }
        if (depth>1) {
            ans+=perftnobulk(depth-1, initialdepth, color^1);
        }
        else {
            ans++;
        }
        unmakemove(moves[depth][i]);
    }
    if (depth == initialdepth-1) {
        cout << ans << " ";
    }
    if (depth == initialdepth) {
        cout << "\n" << ans << "\n";
        auto finish = chrono::steady_clock::now();
        auto diff = chrono::duration_cast<chrono::milliseconds>(finish-start);
        int nps = 1000*(ans/diff.count());
        cout << "nps " << nps << "\n";
    }
    return ans;
}
void parseFEN(string FEN) {
    gamelength = 0;
    last = 0;
    root = 0;
    gamephase[0] = 0;
    gamephase[1] = 0;
    evalm[0] = 0;
    evalm[1] = 0;
    evale[0] = 0;
    evale[1] = 0;
    int order[64] = {56, 57, 58, 59, 60, 61, 62, 63, 48, 49, 50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44, 45, 46, 47,
    32, 33, 34, 35, 36, 37, 38, 39, 24, 25, 26, 27, 28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23,
    8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7};
    char file[8] = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'};
    int progress = 0;
    for (int i = 0; i < 8; i++) {
        Bitboards[i] = 0ULL;
    }
    int tracker = 0;
    int castling = 0;
    int color = 0;
    while (FEN[tracker]!=' ') {
        char hm = FEN[tracker];
        if ('0' <= hm && hm <= '9') {
            int repeat = (int)hm-48;
            progress+=repeat;
        }
        if ('A'<= hm && hm <= 'Z') {
            Bitboards[0]|=(1ULL << order[progress]);
            if (hm == 'P') {
                Bitboards[2] |= (1ULL << order[progress]);
                evalm[0]+=materialm[0];
                evale[0]+=materiale[0];
                evalm[0]+=pstm[0][order[progress]];
                evale[0]+=pste[0][order[progress]];
            }
            if (hm == 'A' || hm == 'B') {
                Bitboards[3] |= (1ULL << order[progress]);
                evalm[0]+=materialm[1];
                evale[0]+=materiale[1];
                evalm[0]+=pstm[1][order[progress]];
                evale[0]+=pste[1][order[progress]];
                gamephase[0]+=1;
            }
            if (hm == 'F' || hm == 'Q') {
                Bitboards[4] |= (1ULL << order[progress]);
                evalm[0]+=materialm[2];
                evale[0]+=materiale[2];
                evalm[0]+=pstm[2][order[progress]];
                evale[0]+=pste[2][order[progress]];
                gamephase[0]+=2;
            }
            if (hm == 'N') {
                Bitboards[5] |= (1ULL << order[progress]);
                evalm[0]+=materialm[3];
                evale[0]+=materiale[3];
                evalm[0]+=pstm[3][order[progress]];
                evale[0]+=pste[3][order[progress]];
                gamephase[0]+=4;
            }
            if (hm == 'R') {
                Bitboards[6] |= (1ULL << order[progress]);
                evalm[0]+=materialm[4];
                evale[0]+=materiale[4];
                evalm[0]+=pstm[4][order[progress]];
                evale[0]+=pste[4][order[progress]];
                gamephase[0]+=6;
            }
            if (hm == 'K') {
                Bitboards[7] |= (1ULL << order[progress]);
                evalm[0]+=pstm[5][order[progress]];
                evale[0]+=pste[5][order[progress]];
            }
            progress++;
        }
        if ('a'<= hm && hm <= 'z') {
            Bitboards[1]|=(1ULL << order[progress]);
            if (hm == 'p') {
                Bitboards[2] |= (1ULL << order[progress]);
                evalm[1]+=materialm[0];
                evale[1]+=materiale[0];
                evalm[1]+=pstm[0][56^order[progress]];
                evale[1]+=pste[0][56^order[progress]];
            }
            if (hm == 'a' || hm == 'b') {
                Bitboards[3] |= (1ULL << order[progress]);
                evalm[1]+=materialm[1];
                evale[1]+=materiale[1];
                evalm[1]+=pstm[1][56^order[progress]];
                evale[1]+=pste[1][56^order[progress]];
                gamephase[1]+=1;
            }
            if (hm == 'f' || hm == 'q') {
                Bitboards[4] |= (1ULL << order[progress]);
                evalm[1]+=materialm[2];
                evale[1]+=materiale[2];
                evalm[1]+=pstm[2][56^order[progress]];
                evale[1]+=pste[2][56^order[progress]];
                gamephase[1]+=2;
            }
            if (hm == 'n') {
                Bitboards[5] |= (1ULL << order[progress]);
                evalm[1]+=materialm[3];
                evale[1]+=materiale[3];
                evalm[1]+=pstm[3][56^order[progress]];
                evale[1]+=pste[3][56^order[progress]];
                gamephase[1]+=4;
            }
            if (hm == 'r') {
                Bitboards[6] |= (1ULL << order[progress]);
                evalm[1]+=materialm[4];
                evale[1]+=materiale[4];
                evalm[1]+=pstm[4][56^order[progress]];
                evale[1]+=pste[4][56^order[progress]];
                gamephase[1]+=6;
            }
            if (hm == 'k') {
                Bitboards[7] |= (1ULL << order[progress]);
                evalm[1]+=pstm[5][56^order[progress]];
                evale[1]+=pste[5][56^order[progress]];
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
    tracker+=6;
    int halfmove = (int)(FEN[tracker])-48;
    tracker++;
    if (FEN[tracker]!=' ') {
        halfmove = 10*halfmove+(int)(FEN[tracker])-48;
    }
    position |= (halfmove << 1);
    zobristhash = scratchzobrist();
    zobrist[0] = zobristhash;
    history[0] = position;
}
string getFEN() {
    int order[64] = {56, 57, 58, 59, 60, 61, 62, 63, 48, 49, 50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44, 45, 46, 47,
    32, 33, 34, 35, 36, 37, 38, 39, 24, 25, 26, 27, 28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23,
    8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7};
    string FEN = "";
    int empt = 0;
    char convert[2][6] = {{'P', 'B', 'Q', 'N', 'R', 'K'}, {'p', 'b', 'q', 'n', 'r', 'k'}};
    int color;
    int piece;
    for (int i = 0; i < 64; i++) {
        color = -1;
        for (int j = 0; j < 2; j++) {
            if (Bitboards[j]&(1ULL << order[i])) {
                color = j;
            }
        }
        if (color >= 0) {
            if (empt > 0) {
                FEN = FEN + (char)(empt+48);
                empt = 0;
            }
            for (int j = 0; j < 6; j++) {
                if (Bitboards[j+2]&(1ULL << order[i])) {
                    piece = j;
                }
            }
            FEN = FEN + (convert[color][piece]);
        }
        else {
            empt++;
            if ((i&7) == 7) {
                FEN = FEN + (char)(empt+48);
                empt = 0;
            }
        }
        if (((i&7) == 7) && (i < 63)) {
            FEN = FEN + '/';
        }
    }
    FEN = FEN + ' ';
    if (position&1) {
        FEN = FEN + "b - - ";
    }
    else {
        FEN = FEN + "w - - ";
    }
    int halfmove = position >> 1;
    string bruh = "";
    while (halfmove > 0) {
        bruh = bruh + (char)(halfmove%10+48);
        halfmove /= 10;
    }
    reverse(bruh.begin(), bruh.end());
    FEN = FEN + bruh + " 1";
    return FEN;
}
int evaluate(int color) {
    int midphase = min(48, gamephase[0]+gamephase[1]);
    int endphase = 48-midphase;
    int mideval = evalm[color]-evalm[color^1];
    int endeval = evale[color]-evale[color^1];
    int progress = 200-(position >> 1);
    int pawnblock = popcount(shift_n(Bitboards[2]&Bitboards[0])&Bitboards[1])+popcount(shift_s(Bitboards[2]&Bitboards[1])&Bitboards[0]);
    int base = (mideval*midphase+(block[pawnblock]*endeval*endphase)/24)/48+10;
    return (base*progress)/200;
}
int quiesce(int alpha, int beta, int color, int depth) {
    int score = evaluate(color);
    int bestscore = -30000;
    int movcount;
    if (depth > 3) {
        return score;
    }
    if (checkers(color)) {
        movcount = generatemoves(color, 0, maxdepth+depth);
        if (movcount == 0) {
            return -27000;
        }
    }
    else {
        bestscore = score;
        if (alpha < score) {
            alpha = score;
        }
        if (score >= beta) {
            return score;
        }
        movcount = generatemoves(color, 1, maxdepth+depth);
    }
    if (depth == 0) {
        for (int i = 0; i < movcount; i++) {
            int j = i;
            int temp1 = 0;
            int temp2 = 0;
            while (j > 0 && movescore[maxdepth+depth][j] > movescore[maxdepth+depth][j-1]) {
                swap(moves[maxdepth+depth][j], moves[maxdepth+depth][j-1]);
                swap(movescore[maxdepth+depth][j], movescore[maxdepth+depth][j-1]);
                j--;
            }
        }
    }
    for (int i = 0; i < movcount; i++) {
        makemove(moves[maxdepth+depth][i], 1);
        score = -quiesce(-beta, -alpha, color^1, depth+1);
        unmakemove(moves[maxdepth+depth][i]);
        if (score >= beta) {
            return score;
        }
        if (score > alpha) {
            alpha = score;
        }
        if (score > bestscore) {
            bestscore = score;
        }
    }
    return bestscore;
}
int alphabeta(int depth, int initialdepth, int alpha, int beta, int color, bool nmp, int nodelimit, int timelimit) {
    if (repetitions() > 1) {
        return 0;
    }
    if ((Bitboards[0]|Bitboards[1]) == Bitboards[7]) {
        return 0;
    }
    if ((Bitboards[color]&Bitboards[7]) == Bitboards[color]) {
        return -1*(depth+28000-initialdepth);
    }
    if (depth == 0) {
        return quiesce(alpha, beta, color, 0);
    }
    int score = -30000;
    int bestscore = -30000;
    int allnode = 0;
    int movcount;
    int index = zobristhash%TTsize;
    int ttmove = 0;
    int bestmove1 = -1;
    int ttdepth = TT[index].depth;
    int ttage = max(gamelength-TT[index].age, 0);
    bool update = (depth > (ttdepth-ttage/3));
    bool incheck = (checkers(color) != 0ULL);
    if (TT[index].key == zobristhash) {
        score = TT[index].score;
        ttmove = TT[index].hashmove;
        int nodetype = TT[index].nodetype;
        if (ttdepth >= depth) {
            if (bestmove >= 0 && repetitions() == 0) {
                if (nodetype == 3) {
                    return score;
                }
                if ((nodetype&1)&&(score >= beta)) {
                    return score;
                }
                if ((nodetype&2)&&(score <= alpha)) {
                    return score;
                }
            }
        }
        else {
            int margin = 40+60*(depth-ttdepth);
            if ((nodetype&1) && (score-margin >= beta) && abs(beta) < 27000) {
                return score-margin;
            }
        }
    }
    int margin = 50+70*depth;
    if (depth < initialdepth && score == -30000) {
        if (evaluate(color)-margin >= beta && abs(beta) < 400) {
            return evaluate(color)-margin;
        }
    }
    movcount = generatemoves(color, 0, depth);
    if (movcount == 0) {
        return -1*(depth+28000-initialdepth);
    }
    if ((!incheck && gamephase[color] > 0) && (depth > 2 && depth < initialdepth) && nmp) {
        makenullmove();
        score = -alphabeta(depth-1-(depth+1)/3, initialdepth, -beta, 1-beta, color^1, false, nodelimit, timelimit);
        unmakenullmove();
        if (score >= beta) {
            return beta;
        }
    }
    if (depth > 1) {
        for (int i = 0; i < movcount; i++) {
            int j = i;
            int temp1 = 0;
            int temp2 = 0;
            if (moves[depth][i] == ttmove) {
                movescore[depth][i] = (1 << 20);
            }
            if (moves[depth][i] == killers[depth][0]) {
                movescore[depth][i]+=20000;
            }
            if (moves[depth][i] == killers[depth][1]) {
                movescore[depth][i]+=10000;
            }
            while (j > 0 && movescore[depth][j] > movescore[depth][j-1]) {
                swap(moves[depth][j], moves[depth][j-1]);
                swap(movescore[depth][j], movescore[depth][j-1]);
                j--;
            }
        }
    }
    for (int i = 0; i < movcount; i++) {
        bool nullwindow = (i > 0);
        if (!stopsearch) {
            makemove(moves[depth][i], true);
            if (nullwindow) {
                score = -alphabeta(depth-1, initialdepth, -alpha-1, -alpha, color^1, true, nodelimit, timelimit);
                if (score > alpha && score < beta) {
                    score = -alphabeta(depth-1, initialdepth, -beta, -alpha, color^1, true, nodelimit, timelimit);
                }
            }
            else {
                score = -alphabeta(depth-1, initialdepth, -beta, -alpha, color^1, true, nodelimit, timelimit);
            }
            unmakemove(moves[depth][i]);
            if (score > bestscore) {
                if (score > alpha) {
                    if (score >= beta) {
                        if (update && !stopsearch && abs(score) < 29000) {
                            updatett(index, depth, score, 1, moves[depth][i]);
                        }
                        if (((moves[depth][i]&1) == 0) && (killers[depth][0] != moves[depth][i])) {
                            killers[depth][1] = killers[depth][0];
                            killers[depth][0] = moves[depth][i];
                        }
                        int target = (moves[depth][i]>>6)&63;
                        int piece = (moves[depth][i]>>13)&7;
                        historytable[color][piece-2][target]+=(depth*depth*depth);
                        return score;
                    }
                    alpha = score;
                    allnode = 1;
                }
                if (depth == initialdepth) {
                    bestmove = moves[depth][i];
                }
                bestmove1 = i;
                bestscore = score;
            }
            if (nodecount > nodelimit) {
                stopsearch = true;
            }
            if (depth > 3) {
                auto now = chrono::steady_clock::now();
                auto timetaken = chrono::duration_cast<chrono::milliseconds>(now - start);
                if (timetaken.count() > timelimit) {
                    stopsearch = true;
                }
            }
        }
    }
    if ((update && !stopsearch) && ((bestmove1 >= 0) && (abs(bestscore) < 29000))) {
        updatett(index, depth, bestscore, 2+allnode, moves[depth][bestmove1]);
    }
    return bestscore;
}
void iterative(int nodelimit, int timelimit, int color) {
    nodecount = 0;
    stopsearch = false;
    start = chrono::steady_clock::now();
    int score = evaluate(color);
    int depth = 1;
    int bestmove1 = 0;
    int pvtable[maxdepth];
    resethistory();
    while (!stopsearch) {
        bestmove = -1;
        int delta = 30;
        int alpha = score-delta;
        int beta = score+delta;
        bool fail = true;
        while (fail) {
            int score1 = alphabeta(depth, depth, alpha, beta, color, true, nodelimit, timelimit);
            if (score1 >= beta) {
                beta += delta;
                delta += delta;
            }
            else if (score1 <= alpha) {
                alpha -= delta;
                delta += delta;
            }
            else {
                score = score1;
                fail = false;
            }
        }
        auto now = chrono::steady_clock::now();
        auto timetaken = chrono::duration_cast<chrono::milliseconds>(now-start);
        if (nodecount < nodelimit && timetaken.count() < timelimit && depth < maxdepth && bestmove >= 0) {
            int last = depth;
            int pvcolor = color;
            bool stop = false;
            for (int i = 0; i < depth; i++) {
                int index = zobristhash%TTsize;
                stop = true;
                if (TT[index].key == zobristhash && TT[index].nodetype == 3) {
                    int movcount = generatemoves(pvcolor, 0, 0);
                    for (int j = 0; j < movcount; j++) {
                        if (moves[0][j] == TT[index].hashmove) {
                            stop = false;
                        }
                    }
                }
                if (stop) {
                    last = i;
                    i = depth;
                }
                else {
                    pvcolor^=1;
                    pvtable[i] = TT[index].hashmove;
                    makemove(TT[index].hashmove, 1);
                }
            }
            for (int i = last-1; i >= 0; i--) {
                unmakemove(pvtable[i]);
            }
            if (proto == "uci") {
                if (abs(score) <= 27000) {
                    cout << "info depth " << depth << " nodes " << nodecount << " time " << timetaken.count() << " score cp " << score << " pv ";
                    for (int i = 0; i < last; i++) {
                        cout << algebraic(pvtable[i]) << " ";
                    }
                    cout << "\n";
                }
                else {
                    int matescore;
                    if (score > 0) {
                        matescore = 1+(28000-score)/2;
                    }
                    else {
                        matescore = (-28000-score)/2;
                    }
                    cout << "info depth " << depth << " nodes " << nodecount << " time " << timetaken.count() << " score mate " << matescore << " pv ";
                    for (int i = 0; i < last; i++) {
                        cout << algebraic(pvtable[i]) << " ";
                    }
                    cout << "\n";
                }
            }
            if (proto == "xboard") {
                cout << depth << " " << score << " " << timetaken.count()/10 << " " << nodecount << " ";
                for (int i=0; i < last; i++) {
                    cout << algebraic(pvtable[i]) << " ";
                }
                cout << "\n";
            }
            depth++;
            bestmove1 = bestmove;
        }
        else {
            stopsearch = true;
        }
    }
    auto now = chrono::steady_clock::now();
    auto timetaken = chrono::duration_cast<chrono::milliseconds>(now-start);
    if (timetaken.count() > 0 && proto == "uci") {
        int nps = 1000*(nodecount/timetaken.count());
        cout << "info nodes " << nodecount << " nps " << nps << "\n";
    }
    if (proto == "uci") {
        cout << "bestmove " << algebraic(bestmove1) << "\n";
    }
    if (proto == "xboard") {
        cout << "move " << algebraic(bestmove1) << "\n";
        makemove(bestmove1, 0);
    }
}
void bookgen(int a, int b, int depth) {
    int color = position&1;
    if (depth == 0) {
        int eval = quiesce(-29000, 29000, color, 0);
        if (eval > a && eval < b) {
            bookoutput << getFEN() << "\n";
        }
    }
    else {
        int movcount = generatemoves(color, 0, depth);
        for (int i = 0; i < movcount; i++) {
            makemove(moves[depth][i], 1);
            bookgen(-b, -a, depth-1);
            unmakemove(moves[depth][i]);
        }
    }
}
void uci() {
    string ucicommand;
    getline(cin, ucicommand);
    if (ucicommand == "uci") {
        cout << "id name sscg13 chess engine \n" << "id author sscg13 \n" << "uciok\n";
    }
    if (ucicommand == "quit") {
        exit(0);
    }
    if (ucicommand == "isready") {
        cout << "readyok\n";
    }
    if (ucicommand == "ucinewgame") {
        initializett();
        initializeboard();
    }
    if (ucicommand.substr(0, 17) == "position startpos") {
        initializeboard();
        int color = 0;
        string mov = "";
        for (int i = 24; i <= ucicommand.length(); i++) {
            if ((ucicommand[i]==' ') || (i == ucicommand.length())) {
                int len = generatemoves(color, 0, 0);
                int played = -1;
                for (int j = 0; j < len; j++) {
                    if (algebraic(moves[0][j])==mov) {
                        played = j;
                    }
                }
                if (played >= 0) {
                    makemove(moves[0][played], false);
                    color^=1;
                }
                mov = "";
            }
            else {
                mov+=ucicommand[i];
            }
        }
    }
    if (ucicommand.substr(0, 12) == "position fen") {
        int reader = 13;
        while (ucicommand[reader]!='m' && reader < ucicommand.length()) {
            reader++;
        }
        string fen = ucicommand.substr(13, reader-12);
        parseFEN(fen);
        int color = position&1;
        string mov = "";
        for (int i = reader+6; i <= ucicommand.length(); i++) {
            if ((ucicommand[i]==' ') || (i == ucicommand.length())) {
                int len = generatemoves(color, 0, 0);
                int played = -1;
                for (int j = 0; j < len; j++) {
                    if (algebraic(moves[0][j])==mov) {
                        played = j;
                    }
                }
                if (played >= 0) {
                    makemove(moves[0][played], false);
                    color^=1;
                }
                mov = "";
            }
            else {
                mov+=ucicommand[i];
            }
        }
    }
    if (ucicommand.substr(0, 8) == "go wtime") {
        int wtime;
        int btime;
        int winc = 0;
        int binc = 0;
        int sum;
        int add;
        int reader = 8;
        while (ucicommand[reader] != 'b') {
            reader++;
        }
        reader--;
        while (ucicommand[reader] == ' ') {
            reader--;
        }
        sum = 0;
        add = 1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        if (sum < 100) {
            sum = 100;
        }
        wtime = sum;
        while (ucicommand[reader] != 'w' && reader < ucicommand.length()) {
            reader++;
        }
        reader--;
        while (ucicommand[reader] == ' ') {
            reader--;
        }
        sum = 0;
        add = 1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        if (sum < 100) {
            sum = 100;
        }
        btime = sum;
        while (ucicommand[reader] != 'b' && reader < ucicommand.length()) {
            reader++;
        }
        reader--;
        while (ucicommand[reader] == ' ') {
            reader--;
        }
        if (reader < ucicommand.length()-1) {
            sum = 0;
            add = 1;
            while (ucicommand[reader] != ' ') {
                sum+=((int)(ucicommand[reader]-48))*add;
                add*=10;
                reader--;
            }
            winc = sum;
            while (ucicommand[reader] != 'b') {
                reader++;
            }
            reader--;
            while (ucicommand[reader] != ' ') {
                reader--;
            }
            sum = 0;
            add = 1;
            while (ucicommand[reader] != ' ') {
                sum+=((int)(ucicommand[reader]-48))*add;
                add*=10;
                reader--;
            }
            binc = sum;
        }
        int color = position&1;
        if (color == 0) {
            iterative(1000000000, wtime/35+winc/3, 0);

        }
        else {
            iterative(1000000000, btime/35+binc/3, 1);
        }
    }
    if (ucicommand.substr(0, 11) == "go movetime") {
        int sum = 0;
        int add = 1;
        int reader = ucicommand.length()-1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        int color = position&1;
        iterative(1000000000, sum, color);
    }
    if (ucicommand.substr(0, 8) == "go nodes") {
        int sum = 0;
        int add = 1;
        int reader = ucicommand.length()-1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        int color = position&1;
        iterative(sum, 120000, color);
    }
    if (ucicommand.substr(0, 11) == "go infinite") {
        int color = position&1;
        iterative(1000000000, 120000, color);
    }
    if (ucicommand.substr(0, 8) == "go perft") {
        start = chrono::steady_clock::now();
        int color = position&1;
        int sum = 0;
        int add = 1;
        int reader = ucicommand.length()-1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        perft(sum, sum, color);
    }
    if (ucicommand.substr(0, 9) == "go sperft") {
        start = chrono::steady_clock::now();
        int color = position&1;
        int sum = 0;
        int add = 1;
        int reader = ucicommand.length()-1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        perftnobulk(sum, sum, color);
    }
    if (ucicommand.substr(0, 8) == "get book") {
        bookoutput.open("shatranj book.txt", ofstream::app);
        int sum = 0;
        int add = 1;
        int reader = 9;
        while (ucicommand[reader] != ' ') {
            reader++;
        }
        int reader2 = reader-1;
        sum = 0;
        add = 1;
        while (ucicommand[reader2] != ' ') {
            sum+=((int)(ucicommand[reader2]-48))*add;
            add*=10;
            reader2--;
        }
        reader++;
        while (ucicommand[reader] != ' ') {
            reader++;
        }
        reader--;
        int a = sum;
        sum = 0;
        add = 1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        int b = sum;
        int depth = (int)(ucicommand[ucicommand.length()-1]-48);
        cout << a << " " << b << " " << depth << "\n";
        bookgen(a, b, depth);
        cout << "done \n";
        bookoutput.close();
    }
}
void xboard() {
    string xcommand;
    getline(cin, xcommand);
    if (xcommand.substr(0, 8) == "protover") {
        cout << "feature ping=1 setboard=1 analyze=0 sigint=0 sigterm=0 myname=\"sscg13 engine\" variants=\"shatranj\" done=1\n";
    }
    if (xcommand == "new") {
        initializett();
        initializeboard();
        gosent = false;
    }
    if (xcommand.substr(0, 8) == "setboard") {
        string fen = xcommand.substr(9, xcommand.length()-9);
        parseFEN(fen);
    }
    if (xcommand.substr(0, 4) == "time") {
        int reader = 5;
        while ('0' <= xcommand[reader] && xcommand[reader] <= '9') {
            reader++;
        }
        reader--;
        int sum = 0;
        int add = 10;
        while (xcommand[reader] != ' ') {
            sum+=((int)(xcommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        movetime = sum/29;
    }
    if (xcommand.substr(0, 7) == "level 0") {
        int reader = 8;
        int sum1 = 0;
        int sum2 = 0;
        movetime = 0;
        int add = 60000;
        while ((xcommand[reader] != ' ') && (xcommand[reader] != ':')) {
            reader++;
        }
        int save = reader;
        reader--;
        while (xcommand[reader] != ' ') {
            sum1+=((int)(xcommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        add = 10000;
        reader = save+1;
        if (xcommand[save] == ':') {
            while (xcommand[reader] != ' ') {
                sum1+=((int)(xcommand[reader]-48))*add;
                add/=10;
                reader++;
            }
        }
        add = 1000;
        bool incenti = false;
        reader = xcommand.length()-1;
        while (xcommand[reader] != ' ') {
            if (xcommand[reader] >= '0') {
                sum2+=((int)xcommand[reader]-48)*add;
                add*=10;
            }
            if (xcommand[reader] == '.') {
                incenti = true;
            }
            reader--;
        }
        if (incenti) {
            sum2/=100;
        }
        movetime = sum1/35+sum2/3;
    }
    if (xcommand.substr(0, 4) == "ping") {
        int sum = 0;
        int add = 1;
        int reader = xcommand.length()-1;
        while (xcommand[reader] != ' ') {
            sum+=((int)(xcommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        cout << "pong " << sum << "\n";
    }
    if ((xcommand.length() == 4) || (xcommand.length() == 5)) {
        int color = position&1;
        int len = generatemoves(color, 0, 0);
        int played = -1;
        for (int j = 0; j < len; j++) {
            if (algebraic(moves[0][j])==xcommand) {
                played = j;
            }
        }
        if (played >= 0) {
            makemove(moves[0][played], false);
            if (gosent) {
                int color = position&1;
                iterative(1000000000, movetime, color);
            }
        }
    }
    if (xcommand == "go") {
        int color = position&1;
        iterative(1000000000, movetime, color);
        gosent = true;
    }
}
int main() {
    initializeleaperattacks();
    initializemasks();
    initializerankattacks();
    initializeboard();
    initializezobrist();
    initializelmr();
    initializett();
    srand(time(0));
    getline(cin, proto);
    if (proto == "uci") {
        cout << "id name sscg13 chess engine \n" << "id author sscg13 \n" << "option name UCI_Variant type combo default shatranj var shatranj \n" << "uciok\n";
        while (true) {
            uci();
        }
    }
    if (proto == "xboard") {
        while (true) {
            xboard();
        }
    }
    return 0;
}
