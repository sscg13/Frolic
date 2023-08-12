#include <iostream>
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
//White, Black, Pawn, Knight, Bishop, Rook, Queen, King
U64 AntiDiags[15] = {1ULL, 0x0000000000000102, 0x0000000000010204, 0x0000000001020408, 0x0000000102040810,
0x0000010204081020, 0x0001020408102040, 0x0102040810204080, 0x0204081020408000, 0x0408102040800000,
0x0810204080000000, 0x1020408000000000, 0x2040800000000000, 0x4080000000000000, 0x8000000000000000};
U64 Diags[15] = {0x0100000000000000, 0x0201000000000000,0x0402010000000000, 0x0804020100000000, 0x1008040201000000,
0x2010080402010000, 0x4020100804020100, 0x8040201008040201, 0x0080402010080402, 0x0000804020100804,
0x0000008040201008, 0x0000000080402010, 0x0000000000804020, 0x0000000000008040, 0x0000000000000080};
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
U64 zobrist[1024];
int history[1024];
int gamelength = 0;
int last = 0;
int root = 0;
int moves[64][256];
int movescore[64][256];
int maxdepth = 32;
int position = 0;
int evalm[2] = {0, 0};
int evale[2] = {0, 0};
int nodecount = 0;
int bestmove = 0;
U64 zobristhash = 0ULL;
bool randomeval = false;
bool stopsearch = false;
bool suppressoutput = false;
//1 bit color, 7 bits halfmove, 6 bits ep, 4 bits castling KQkq
//6 bits from square, 6 bits to square, 1 bit color, 3 bits piece moved, 1 bit castling, 1 bit double pawn push,
//1 bit en passant, 1 bit promotion, 2 bits promoted piece, 1 bit capture, 3 bits piece captured
//26 bits total for now?
int movecount;
auto start = chrono::steady_clock::now();
int materialm[6] = {82, 337, 365, 477, 735, 20000};
int materiale[6] = {94, 371, 347, 652, 1036, 20000};
int pstm[6][64] = {{0,0,0,0,0,0,0,0,-35,-1,-20,-23,-15,24,38,-22,-26,-4,-4,-10,3,3,33,-12,-27,-2,-5,12,17,6,10,-25,
-14,13,6,21,23,12,17,-23,-6,7,26,31,65,56,25,-20,98,134,61,95,68,126,34,-11,0,0,0,0,0,0,0,0},
{-105,-21,-58,-33,-17,-28,-19,-23,-29,-53,-12,-3,-1,18,-14,-19,-23,-9,12,10,19,17,25,-16,-13,4,16,13,28,19,21,-8,
-9,17,19,53,37,69,18,22,-47,60,37,65,84,129,73,44,-73,-41,72,36,23,62,7,-17,-167,-89,-34,-49,61,-97,-15,-107},
{-33,-3,-14,-21,-13,-12,-39,-21,4,15,16,0,7,21,33,1,0,15,15,15,14,27,18,10,-6,13,13,26,34,12,10,4,
-4,5,19,50,37,37,7,-2,-16,37,43,40,35,50,37,-2,-26,16,-18,-13,30,59,18,-47,-29,4,-82,-37,-25,-42,7,-8},
{-19,-13,1,17,16,7,-37,-26,-44,-16,-20,-9,-1,11,-6,-71,-45,-25,-16,-17,3,0,-5,-33,-36,-26,-12,-1,9,-7,6,-23,
-24,-11,7,26,24,35,-8,-20,-5,19,26,36,17,45,61,16,27,32,58,62,80,67,26,44,32,42,32,51,63,9,31,43},
{-1,-18,-9,10,-15,-25,-31,-50,-35,-8,11,2,8,15,-3,1,-14,2,-11,-2,-5,2,14,5,-9,-26,-9,-10,-2,-4,3,-3,
-27,-27,-16,-16,-1,17,-2,1,-13,-17,7,8,29,56,47,57,-24,-39,-5,1,-16,57,28,54,-28,0,29,12,59,44,43,45},
{-15,36,12,-54,8,-28,34,14,1,7,-8,-64,-43,-16,9,8,-14,-14,-22,-46,-44,-30,-15,-27,-49,-1,-27,-39,-46,-44,-33,-51,
-17,-20,-12,-27,-30,-25,-14,-36,-9,24,2,-16,-20,6,22,-22,29,-1,-20,-7,-8,-4,-38,-29,-65,23,16,-15,-56,-34,2,13}};
int pste[6][64] = {{0,0,0,0,0,0,0,0,13,8,8,10,13,0,2,-7,4,7,-6,1,0,-5,-1,-8,13,9,-3,-7,-7,-8,3,-1,
32,24,13,5,-2,4,17,17,94,100,85,67,56,53,82,84,178,173,158,134,147,132,165,187,0,0,0,0,0,0,0,0},
{-29,-51,-23,-15,-22,-18,-50,-64,-42,-20,-10,-5,-2,-20,-23,-44,-23,-3,-1,15,10,-3,-20,-22,-18,-6,16,25,16,17,4,-18,
-17,3,22,22,22,11,8,-18,-24,-20,10,9,-1,-9,-19,-41,-25,-8,-25,-2,-9,-25,-24,-52,-58,-38,-13,-28,-31,-27,-63,-99},
{-23,-9,-23,-5,-9,-16,-5,-17,-14,-18,-7,-1,4,-9,-15,-27,-12,-3,8,10,13,3,-7,-15,-6,3,13,19,7,10,-3,-9,
-3,9,12,9,14,10,3,2,2,-8,0,-1,-2,6,0,4,-8,-4,7,-12,-3,-13,-4,-14,-14,-21,-11,-8,-7,-9,-17,-24},
{-9,2,3,-1,-5,-13,4,-20,-6,-6,0,2,-9,-9,-11,-3,-4,0,-5,-1,-7,-12,-8,-16,3,5,8,4,-5,-6,-8,-11,
4,3,13,1,2,1,-1,2,7,7,7,5,4,-3,-5,-3,11,13,13,11,-3,3,8,3,13,10,18,15,12,12,8,5},
{-33,-28,-22,-43,-5,-32,-20,-41,-22,-23,-30,-16,-16,-23,-36,-32,-16,-27,15,6,9,17,10,5,-18,28,19,47,31,34,39,23,
3,22,24,45,57,40,57,36,-20,6,9,49,47,35,19,9,-17,20,32,41,58,25,30,0,-9,22,22,27,27,19,10,20},
{-53,-34,-21,-11,-28,-14,-24,-43,-27,-11,4,13,14,4,-5,-17,-19,-3,11,21,23,16,7,-9,-18,-4,21,24,27,23,9,-11,
-8,22,24,27,26,33,26,3,10,17,23,15,20,45,44,13,-12,17,14,17,17,38,23,11,-74,-35,-18,-18,-11,15,4,-17}};
int castlechange[64] = {13, 15, 15, 15, 12, 15, 15, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
15, 15, 15, 15,15, 15, 15, 15, 15, 15, 15, 15,15, 15, 15, 15, 15, 15, 15, 15,15, 15, 15, 15, 15, 15, 15, 15,7, 15, 15, 15,  3, 15, 15, 11};
int historytable[2][6][64];
int startpiece[16] = {3, 1, 2, 4, 5, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0};
int phase[6] = {0, 1, 1, 2, 4, 0};
int gamephase[2] = {0, 0};
ofstream gameoutput;
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
    }
}
void initializemasks() {
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            RankMask[8*i+j] = Ranks[i];
            FileMask[8*i+j] = Files[j];
            AntiMask[8*i+j] = AntiDiags[i+j];
            DiagMask[8*i+j] = Diags[j+7-i];
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
U64 PawnMoves(U64 occupied, int square, int color) {
    int row = square&56;
    U64 step1;
    if (color == 0) {
        step1 = (1ULL << (square+8))&(~occupied);
    }
    else {
        step1 = (1ULL << (square-8))&(~occupied);
    }
    if ((row != 8 && color == 0)||(row!=48 && color == 1)) {
        return step1;
    }
    U64 step2;
    if (color == 0) {
        step2 = (step1<<8)&(~occupied);
    }
    else {
        step2 = (step1>>8)&(~occupied);
    }
    return step1|step2;
}
U64 DiagAttacks(U64 occupied, int square) {
    U64 forwards = occupied & DiagMask[square];
    U64 backwards = byteswap(forwards);
    forwards = forwards - 2*(1ULL << square);
    backwards = backwards - 2*(1ULL << (56^square));
    backwards = byteswap(backwards);
    return (forwards^backwards) & DiagMask[square];
}
U64 AntiAttacks(U64 occupied, int square) {
    U64 forwards = occupied & AntiMask[square];
    U64 backwards = byteswap(forwards);
    forwards = forwards - 2*(1ULL << square);
    backwards = backwards - 2*(1ULL << (56^square));
    backwards = byteswap(backwards);
    return (forwards^backwards) & AntiMask[square];
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
        epfilehash[i] = mt();
    }
    for (int i = 0; i < 16; i++) {
        castlinghash[i] = mt();
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
    if (position & 0x00003F00) {
        int file = (position >> 8) & 7;
        scratch^=epfilehash[file];
    }
    int castling = (position >> 14) & 15;
    scratch^=castlinghash[castling];
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
void clearhistory() {
    for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 64; j++) {
            historytable[0][i][j] = 0;
            historytable[1][i][j] = 0;
        }
    }
}
void dampenhistory() {
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
    Bitboards[3] = (Rank1 | Rank8) & (FileB | FileG);
    Bitboards[4] = (Rank1 | Rank8) & (FileC | FileF);
    Bitboards[5] = (Rank1 | Rank8) & (FileA | FileH);
    Bitboards[6] = (Rank1 | Rank8) & FileD;
    Bitboards[7] = (Rank1 | Rank8) & FileE;
    position = 0x0003C000;
    history[0] = position;
    int startmatm = (8*materialm[0]+2*(materialm[1]+materialm[2]+materialm[3])+materialm[4]);
    int startmate = (8*materiale[0]+2*(materiale[1]+materiale[2]+materiale[3])+materiale[4]);
    int startpstm = 0;
    int startpste = 0;
    for (int i = 0; i < 16; i++) {
        startpstm+=pstm[startpiece[i]][i];
        startpste+=pste[startpiece[i]][i];
    }
    evalm[0] = startmatm+startpstm;
    evalm[1] = startmatm+startpstm;
    evale[0] = startmate+startpste;
    evale[1] = startmate+startpste;
    gamephase[0] = 12;
    gamephase[1] = 12;
    gamelength = 0;
    zobrist[0] = scratchzobrist();
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
void makemove(int notation, bool reversible) {
    //6 bits from square, 6 bits to square, 1 bit color, 3 bits piece moved, 1 bit capture, 3 bits piece captured, 1 bit promotion,
    //2 bits promoted piece, 1 bit castling, 1 bit double pawn push, 1 bit en passant,
    //26 bits total

    //1 bit color, 7 bits halfmove, 6 bits ep, 4 bits castling
    gamelength++;
    if (!reversible) {
        root = gamelength;
    }
    if (position & 0x00003F00) {
        int file = (position >> 8) & 7;
        zobristhash^=epfilehash[file];
    }
    int castling = (position >> 14) & 15;
    zobristhash^=castlinghash[castling];
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
    else if (notation & (1 << 23)) {
        if (to&4) {
            Bitboards[color]^=((1ULL << (to-1)) | (1ULL << (to+1)));
            Bitboards[5]^=((1ULL << (to-1)) | (1ULL << (to+1)));
            zobristhash^=(hashes[color][to-1]^hashes[color][to+1]);
            zobristhash^=(hashes[5][to-1]^hashes[5][to+1]);
            evalm[color]+=(pstm[4][7^(56*color)]-pstm[4][5^(56*color)]);
            evale[color]+=(pste[4][7^(56*color)]-pste[4][5^(56*color)]);
        }
        else {
            Bitboards[color]^=((1ULL << (to-2)) | (1ULL << (to+1)));
            Bitboards[5]^=((1ULL << (to-2)) | (1ULL << (to+1)));
            zobristhash^=(hashes[color][to-2]^hashes[color][to+1]);
            zobristhash^=(hashes[5][to-2]^hashes[5][to+1]);
            evalm[color]+=(pstm[4][0^(56*color)]-pstm[4][3^(56*color)]);
            evale[color]+=(pste[4][0^(56*color)]-pste[4][3^(56*color)]);
        }
        evalm[color]+=50;
    }
    else if (notation & (1 << 24)) {
        position^=((from+to) << 7);
    }
    else if (notation & (1 << 25)) {
        int shadow;
        if (color==1) {
            shadow = to+8;
        }
        else {
            shadow = to-8;
        }
        Bitboards[2]^=(1ULL << shadow);
        Bitboards[color^1]^=(1ULL << shadow);
        zobristhash^=(hashes[2][shadow]^hashes[color^1][shadow]);
        evalm[color^1]-=(materialm[0]+pstm[0][(56*(color^1))^shadow]);
        evale[color^1]-=(materiale[0]+pste[0][(56*(color^1))^shadow]);
    }
    position&=(0x00003FFF | (castlechange[from] << 14));
    position&=(0x00003FFF | (castlechange[to] << 14));
    position^=(gamelength&1);
    position^=(halfmove << 1);
    zobristhash^=(colorhash*(gamelength&1));
    if (position & 0x00003F00) {
        int file = (position >> 8) & 7;
        zobristhash^=epfilehash[file];
    }
    castling = (position >> 14) & 15;
    zobristhash^=castlinghash[castling];
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
    else if (notation & (1 << 23)) {
        if (to&4) {
            Bitboards[color]^=((1ULL << (to-1)) | (1ULL << (to+1)));
            Bitboards[5]^=((1ULL << (to-1)) | (1ULL << (to+1)));
            evalm[color]+=(pstm[4][5^(56*color)]-pstm[4][7^(56*color)]);
            evale[color]+=(pste[4][5^(56*color)]-pste[4][7^(56*color)]);
        }
        else {
            Bitboards[color]^=((1ULL << (to-2)) | (1ULL << (to+1)));
            Bitboards[5]^=((1ULL << (to-2)) | (1ULL << (to+1)));
            evalm[color]+=(pstm[4][3^(56*color)]-pstm[4][0^(56*color)]);
            evale[color]+=(pste[4][3^(56*color)]-pste[4][0^(56*color)]);
        }
        evalm[color]-=50;
    }
    else if (notation & (1 << 25)) {
        int shadow;
        if (color) {
            shadow = to+8;
        }
        else {
            shadow = to-8;
        }
        Bitboards[2]^=(1ULL << shadow);
        Bitboards[color^1]^=(1ULL << shadow);
        evalm[color^1]+=(materialm[0]+pstm[0][(56*(color^1))^shadow]);
        evale[color^1]+=(materiale[0]+pste[0][(56*(color^1))^shadow]);
    }
}
int generatemoves(int color, int depth) {
    movecount = 0;
    int kingsquare = popcount((Bitboards[color] & Bitboards[7])-1);
    int pinrank = kingsquare&56;
    int pinfile = kingsquare&7;
    int opposite = color^1;
    U64 preoccupied = Bitboards[0] | Bitboards[1];
    U64 kingDiag = DiagAttacks(preoccupied, kingsquare);
    U64 kingAnti = AntiAttacks(preoccupied, kingsquare);
    U64 kingRank = GetRankAttacks(preoccupied, kingsquare);
    U64 kingFile = FileAttacks(preoccupied, kingsquare);
    U64 occupied = preoccupied^(1ULL << kingsquare);
    U64 opponentpawns = Bitboards[opposite]&Bitboards[2];
    U64 opponentknights = Bitboards[opposite]&Bitboards[3];
    U64 opponentbishops = Bitboards[opposite]&Bitboards[4];
    U64 opponentrooks = Bitboards[opposite]&Bitboards[5];
    U64 opponentqueens = Bitboards[opposite]&Bitboards[6];
    int pawncount = popcount(opponentpawns);
    int knightcount = popcount(opponentknights);
    int bishopcount = popcount(opponentbishops);
    int rookcount = popcount(opponentrooks);
    int queencount = popcount(opponentqueens);
    U64 ourcaptures = 0ULL;
    U64 ourmoves = 0ULL;
    U64 ourmask = 0ULL;
    ourcaptures = KingAttacks[kingsquare]&Bitboards[opposite];
    int capturenumber = popcount(ourcaptures);
    int movenumber;
    for (int i = 0; i < capturenumber; i++) {
        int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
        int notation = kingsquare | (capturesquare << 6);
        notation |= (color << 12);
        notation |= (7 << 13);
        int captured = 0;
        for (int j = 2; j < 8; j++) {
            if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                captured = j;
            }
        }
        notation |= (1 << 16);
        notation |= (captured << 17);
        moves[depth][movecount] = notation;
        movescore[depth][movecount] = 300+1000*captured+historytable[color][5][capturesquare];
        movecount++;
        ourcaptures^=(1ULL << capturesquare);
    }
    ourmoves = KingAttacks[kingsquare]&(~preoccupied);
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
    U64 ourpawns = Bitboards[color]&Bitboards[2];
    U64 ourknights = Bitboards[color]&Bitboards[3];
    U64 ourbishops = Bitboards[color]&Bitboards[4];
    U64 ourrooks = Bitboards[color]&Bitboards[5];
    U64 ourqueens = Bitboards[color]&Bitboards[6];
    pawncount = popcount(ourpawns);
    knightcount = popcount(ourknights);
    bishopcount = popcount(ourbishops);
    rookcount = popcount(ourrooks);
    queencount = popcount(ourqueens);
    for (int i = 0; i < pawncount; i++) {
        int pawnsquare = popcount((ourpawns & -ourpawns)-1);
        int epsquare = (position >> 8)&63;
        ourcaptures = PawnAttacks[color][pawnsquare]&(Bitboards[opposite]|(epsquare&(~1ULL)));
        int eptake;
        if (color == 0) {
            eptake = epsquare-8;
        }
        else {
            eptake = epsquare+8;
        }
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = pawnsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (2 << 13);
            int captured = 0;
            for (int j = 2; j < 8; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            if (captured > 0) {
                notation |= (1 << 16);
                notation |= (captured << 17);
            }
            else {
                notation |= (1 << 25);
            }
            if (((color==0)&&(capturesquare&56)==56)||((color==1)&&(capturesquare&56)==0)) {
                for (int k = 0; k < 4; k++) {
                    moves[depth][movecount]=notation|((1 << 20)|(k << 21));
                    movescore[depth][movecount] = 9000+1000*captured+historytable[color][0][capturesquare];
                    movecount++;
                }
            }
            else {
                moves[depth][movecount] = notation;
                movescore[depth][movecount] = 800+1000*captured+historytable[color][0][capturesquare];
                movecount++;
            }
            ourcaptures^=(1ULL << capturesquare);
        }
        ourmoves = PawnMoves(preoccupied, pawnsquare, color);
        int movenumber = popcount(ourmoves);
        for (int j = 0; j < movenumber; j++) {
            int movesquare = popcount((ourmoves & -ourmoves)-1);
            int notation = pawnsquare | (movesquare << 6);
            notation |= (color << 12);
            notation |= (2 << 13);
            if ((movesquare-pawnsquare==16 && color==0)||(pawnsquare-movesquare==16 && color==1)) {
                notation |= (1 << 24);
            }
            if (((color==0)&&(movesquare&56)==56)||((color==1)&&(movesquare&56)==0)) {
                for (int k = 0; k < 4; k++) {
                    moves[depth][movecount]=notation|((1 << 20)|(k << 21));
                    movescore[depth][movecount] = 9000+historytable[color][0][movesquare];
                    movecount++;
                }
            }
            else {
                moves[depth][movecount]=notation;
                movescore[depth][movecount] = historytable[color][0][movesquare];
                movecount++;
            }
            ourmoves^=(1ULL << movesquare);
        }
        ourpawns^=(1ULL << pawnsquare);
    }
    for (int i = 0; i < knightcount; i++) {
        int knightsquare = popcount((ourknights & -ourknights)-1);
        ourmask = KnightAttacks[knightsquare];
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = knightsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (3 << 13);
            int captured = 0;
            for (int j = 2; j < 8; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 700+1000*captured+historytable[color][1][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        ourmoves = ourmask&(~preoccupied);
        int movenumber = popcount(ourmoves);
        for (int j = 0; j < movenumber; j++) {
            int movesquare = popcount((ourmoves & -ourmoves)-1);
            int notation = knightsquare | (movesquare << 6);
            notation |= (color << 12);
            notation |= (3 << 13);
            moves[depth][movecount]=notation;
            movescore[depth][movecount] = historytable[color][1][movesquare];
            movecount++;
            ourmoves^=(1ULL << movesquare);
        }
        ourknights^=(1ULL << knightsquare);
    }
    for (int i = 0; i < bishopcount; i++) {
        int bishopsquare = popcount((ourbishops & -ourbishops)-1);
        ourmask = (DiagAttacks(preoccupied, bishopsquare)|AntiAttacks(preoccupied, bishopsquare));
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = bishopsquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (4 << 13);
            int captured = 0;
            for (int j = 2; j < 8; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 600+1000*captured+historytable[color][2][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        ourmoves = ourmask&(~preoccupied);
        int movenumber = popcount(ourmoves);
        for (int j = 0; j < movenumber; j++) {
            int movesquare = popcount((ourmoves & -ourmoves)-1);
            int notation = bishopsquare | (movesquare << 6);
            notation |= (color << 12);
            notation |= (4 << 13);
            moves[depth][movecount]=notation;
            movescore[depth][movecount] = historytable[color][2][movesquare];
            movecount++;
            ourmoves^=(1ULL << movesquare);
        }
        ourbishops^=(1ULL << bishopsquare);
    }
    for (int i = 0; i < rookcount; i++) {
        int rooksquare = popcount((ourrooks & -ourrooks)-1);
        ourmask = (GetRankAttacks(preoccupied, rooksquare)|FileAttacks(preoccupied, rooksquare));
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = rooksquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (5 << 13);
            int captured = 0;
            for (int j = 2; j < 8; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 500+1000*captured+historytable[color][3][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        ourmoves = ourmask&(~preoccupied);
        int movenumber = popcount(ourmoves);
        for (int j = 0; j < movenumber; j++) {
            int movesquare = popcount((ourmoves & -ourmoves)-1);
            int notation = rooksquare | (movesquare << 6);
            notation |= (color << 12);
            notation |= (5 << 13);
            moves[depth][movecount]=notation;
            movescore[depth][movecount] = historytable[color][3][movesquare];
            movecount++;
            ourmoves^=(1ULL << movesquare);
        }
        ourrooks^=(1ULL << rooksquare);
    }
    for (int i = 0; i < queencount; i++) {
        int queensquare = popcount((ourqueens & -ourqueens)-1);
        ourmask = (GetRankAttacks(preoccupied, queensquare)|FileAttacks(preoccupied, queensquare));
        ourmask |= (DiagAttacks(preoccupied, queensquare)|AntiAttacks(preoccupied, queensquare));
        ourcaptures = ourmask&Bitboards[opposite];
        int capturenumber = popcount(ourcaptures);
        for (int j = 0; j < capturenumber; j++) {
            int capturesquare = popcount((ourcaptures & -ourcaptures)-1);
            int notation = queensquare | (capturesquare << 6);
            notation |= (color << 12);
            notation |= (6 << 13);
            int captured = 0;
            for (int j = 2; j < 8; j++) {
                if ((1ULL << capturesquare)&(Bitboards[opposite]&Bitboards[j])) {
                    captured = j;
                }
            }
            notation |= (1 << 16);
            notation |= (captured << 17);
            moves[depth][movecount] = notation;
            movescore[depth][movecount] = 400+1000*captured+historytable[color][4][capturesquare];
            movecount++;
            ourcaptures^=(1ULL << capturesquare);
        }
        ourmoves = ourmask&(~preoccupied);
        int movenumber = popcount(ourmoves);
        for (int j = 0; j < movenumber; j++) {
            int movesquare = popcount((ourmoves & -ourmoves)-1);
            int notation = queensquare | (movesquare << 6);
            notation |= (color << 12);
            notation |= (6 << 13);
            moves[depth][movecount]=notation;
            movescore[depth][movecount] = historytable[color][4][movesquare];
            movecount++;
            ourmoves^=(1ULL << movesquare);
        }
        ourqueens^=(1ULL << queensquare);
    }
    if (position&(1 << (14+2*color))) {
        if (!((occupied)&((1ULL << (kingsquare+3))-(1ULL << kingsquare)))) {
            int notation = kingsquare | ((kingsquare+2) << 6);
            notation |= (color << 12);
            notation |= (7 << 13);
            notation |= (1 << 23);
            moves[depth][movecount]=notation;
            movescore[depth][movecount] = 1000+historytable[color][5][kingsquare+2];
            movecount++;
        }
    }
    if (position&(1 << (15+2*color))) {
        if (!(occupied&((1ULL << kingsquare)-(1ULL << (kingsquare-3))))) {
            int notation = kingsquare | ((kingsquare-2) << 6);
            notation |= (color << 12);
            notation |= (7 << 13);
            notation |= (1 << 23);
            moves[depth][movecount]=notation;
            movescore[depth][movecount] = 1000+historytable[color][5][kingsquare-2];
            movecount++;
        }
    }
    return movecount;
}
string algebraic(int notation) {
    string convert[64] = { "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6","a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"};
    string header = convert[notation&63]+convert[(notation>>6)&63];
    if (notation&(1 << 20)) {
        int piece = (notation>>21)&3;
        if (piece == 0) {
            header = header+"n";
        }
        else if (piece == 1) {
            header = header+"b";
        }
        else if (piece == 2) {
            header = header+"r";
        }
        else {
            header = header+"q";
        }
    }
    return header;
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
            if (hm == 'N') {
                Bitboards[3] |= (1ULL << order[progress]);
                evalm[0]+=materialm[1];
                evale[0]+=materiale[1];
                evalm[0]+=pstm[1][order[progress]];
                evale[0]+=pste[1][order[progress]];
                gamephase[0]+=1;
            }
            if (hm == 'B') {
                Bitboards[4] |= (1ULL << order[progress]);
                evalm[0]+=materialm[2];
                evale[0]+=materiale[2];
                evalm[0]+=pstm[2][order[progress]];
                evale[0]+=pste[2][order[progress]];
                gamephase[0]+=1;
            }
            if (hm == 'R') {
                Bitboards[5] |= (1ULL << order[progress]);
                evalm[0]+=materialm[3];
                evale[0]+=materiale[3];
                evalm[0]+=pstm[3][order[progress]];
                evale[0]+=pste[3][order[progress]];
                gamephase[0]+=2;
            }
            if (hm == 'Q') {
                Bitboards[6] |= (1ULL << order[progress]);
                evalm[0]+=materialm[4];
                evale[0]+=materiale[4];
                evalm[0]+=pstm[4][order[progress]];
                evale[0]+=pste[4][order[progress]];
                gamephase[0]+=4;
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
            if (hm == 'n') {
                Bitboards[3] |= (1ULL << order[progress]);
                evalm[1]+=materialm[1];
                evale[1]+=materiale[1];
                evalm[1]+=pstm[1][56^order[progress]];
                evale[1]+=pste[1][56^order[progress]];
                gamephase[1]+=1;
            }
            if (hm == 'b') {
                Bitboards[4] |= (1ULL << order[progress]);
                evalm[1]+=materialm[2];
                evale[1]+=materiale[2];
                evalm[1]+=pstm[2][56^order[progress]];
                evale[1]+=pste[2][56^order[progress]];
                gamephase[1]+=1;
            }
            if (hm == 'r') {
                Bitboards[5] |= (1ULL << order[progress]);
                evalm[1]+=materialm[3];
                evale[1]+=materiale[3];
                evalm[1]+=pstm[3][56^order[progress]];
                evale[1]+=pste[3][56^order[progress]];
                gamephase[1]+=2;
            }
            if (hm == 'q') {
                Bitboards[6] |= (1ULL << order[progress]);
                evalm[1]+=materialm[4];
                evale[1]+=materiale[4];
                evalm[1]+=pstm[4][56^order[progress]];
                evale[1]+=pste[4][56^order[progress]];
                gamephase[1]+=4;
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
    tracker+=2;
    while (FEN[tracker]!=' ') {
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
        tracker+=2;
    }
    else {
        int epfile = 0;
        for (int j = 0; j < 8; j++) {
            if (file[j] == FEN[tracker]) {
                epfile = j;
            }
        }
        tracker++;
        int eprank = (int)(FEN[tracker])-49;
        int epsquare = 8*eprank+epfile;
        position |= (epsquare << 8);
        tracker+=2;
    }
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
int getkingsafety(int color) {
    int king = popcount((Bitboards[7]&Bitboards[color])-1);
    U64 occupied = (Bitboards[0]|Bitboards[1]);
    int diag = ((Bitboards[4]|Bitboards[6])&Bitboards[color^1]) ? 1+popcount(DiagAttacks(occupied, king))/2 : 1;
    int anti = ((Bitboards[4]|Bitboards[6])&Bitboards[color^1]) ? 1+popcount(AntiAttacks(occupied, king))/2 : 1;
    int rnk = ((Bitboards[5]|Bitboards[6])&Bitboards[color^1]) ? 1+popcount(GetRankAttacks(occupied, king))/2 : 1;
    int file = ((Bitboards[5]|Bitboards[6])&Bitboards[color^1]) ? 1+popcount(FileAttacks(occupied, king))/2 : 1;
    int knight = (Bitboards[2]&Bitboards[color^1]) ? 1+popcount(KnightAttacks[king])/2 : 1;
    return diag*anti*rnk*file*knight;
}
int rookfile(int color) {
    U64 ourrooks = Bitboards[color]&Bitboards[5];
    U64 theirrooks = Bitboards[color^1]&Bitboards[5];
    int score = 0;
    while (ourrooks) {
        int rookposition = popcount((ourrooks & -ourrooks)-1);
        if ((Files[rookposition&7]&Bitboards[2]&Bitboards[color]) == 0ULL) {
            if ((Files[rookposition&7]&Bitboards[2]) == 0ULL) {
                score+=130;
            }
            score+=53;
        }
        ourrooks^=(1ULL << rookposition);
    }
    while (theirrooks) {
        int rookposition = popcount((theirrooks & -theirrooks)-1);
        if ((Files[rookposition&7]&Bitboards[2]&Bitboards[color^1]) == 0ULL) {
            if ((Files[rookposition&7]&Bitboards[2]) == 0ULL) {
                score-=130;
            }
            score-=53;
        }
        theirrooks^=(1ULL << rookposition);
    }
    return score;
}
int evaluate(int color) {
    int midphase = min(24, gamephase[0]+gamephase[1]);
    int endphase = 24-midphase;
    int mideval = evalm[color]-evalm[color^1];
    int kingsafety = getkingsafety(color^1)-getkingsafety(color);
    int rooks = rookfile(color)-rookfile(color^1);
    int endeval = evale[color]-evale[color^1];
    int bishops = 37*(popcount(Bitboards[color]&Bitboards[4])/2-popcount(Bitboards[color^1]&Bitboards[4])/2);
    int tempo = (gamelength&1) ? 150 : 0;
    int e = (min(gamephase[0], gamephase[1]) == 0) ? 2 : 1;
    int random = randomeval ? rand()&15 : 0;
    int base = (mideval*midphase+e*endeval*endphase)/24;
    int extra = bishops+tempo+kingsafety+random+rooks;
    return base+extra;
}
int alphabeta(int depth, int initialdepth, int alpha, int beta, int color, bool nmp, int nodelimit, int timelimit) {
    if (repetitions() > 1) {
        return 0;
    }
    int score = -30000;
    int bestscore = -30000;
    if ((Bitboards[color]&Bitboards[7])==0ULL) {
        return -1*(depth+28000-initialdepth);
    }
    if (depth == 0) {
        return evaluate(color);
    }
    int allnode = 0;
    int movcount;
    int index = zobristhash%TTsize;
    int ttmove = 0;
    int bestmove1 = -1;
    int ttdepth = TT[index].depth;
    int ttage = max(gamelength-TT[index].age, 0);
    bool update = (depth > (ttdepth-ttage/3));
    if (TT[index].key == zobristhash) {
        score = TT[index].score;
        ttmove = TT[index].hashmove;
        if (ttdepth == depth) {
            int nodetype = TT[index].nodetype;
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
    }
    movcount = generatemoves(color, depth);
    if (depth > 1) {
        for (int i = 0; i < movcount; i++) {
            int j = i;
            int temp1 = 0;
            int temp2 = 0;
            if (moves[depth][i] == ttmove) {
                movescore[depth][i] = (1 << 20);
            }
            while (j > 0 && movescore[depth][j] > movescore[depth][j-1]) {
                temp1 = moves[depth][j];
                temp2 = movescore[depth][j];
                moves[depth][j] = moves[depth][j-1];
                movescore[depth][j] = movescore[depth][j-1];
                moves[depth][j-1] = temp1;
                movescore[depth][j-1] = temp2;
                j--;
            }
        }
    }
    for (int i = 0; i < movcount; i++) {
        if (!stopsearch) {
            makemove(moves[depth][i], true);
            if (gamelength&1) {
                score = -alphabeta(depth-1, initialdepth, -beta, -alpha, color^1, true, nodelimit, timelimit);
            }
            else {
                score = alphabeta(depth-1, initialdepth, alpha, beta, color, true, nodelimit, timelimit);
            }
            unmakemove(moves[depth][i]);
            if (score > bestscore) {
                if (score > alpha) {
                    if (score >= beta) {
                        if (update && !stopsearch && abs(score) < 29000) {
                            updatett(index, depth, score, 1, moves[depth][i]);
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
            if (depth > 2) {
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
    dampenhistory();
    while (!stopsearch) {
        bestmove = -1;
        score = alphabeta(depth, depth, -29000, 29000, color, 1, nodelimit, timelimit);
        auto now = chrono::steady_clock::now();
        auto timetaken = chrono::duration_cast<chrono::milliseconds>(now-start);
        if (!stopsearch && depth < maxdepth) {
            if (!suppressoutput) {
                cout << "info depth " << depth << " nodes " << nodecount << " time " << timetaken.count() << " score cp " << score << " pv " << algebraic(bestmove) << "\n";
            }
            if (abs(score) > 27000) {
                stopsearch = true;
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
    if (timetaken.count() > 0) {
        int nps = 1000*(nodecount/timetaken.count());
        if (!suppressoutput) {
            cout << "info nodes " << nodecount << " nps " << nps << "\n";
        }
    }
    bestmove = bestmove1;
    if (!suppressoutput) {
        cout << "bestmove " << algebraic(bestmove1) << "\n";
    }
}
void autoplay(int nodes) {
    randomeval = true;
    suppressoutput = true;
    initializett();
    clearhistory();
    initializeboard();
    int rand1 = rand()%20;
    int rand2 = rand()%20;
    generatemoves(0, 0);
    string game = "";
    makemove(moves[0][rand1], 0);
    game += algebraic(moves[0][rand1]);
    game += " ";
    generatemoves(1, 0);
    makemove(moves[0][rand2], 0);
    game += algebraic(moves[0][rand2]);
    game += " ";
    while (popcount(Bitboards[7]) > 1 && repetitions() < 2 && popcount(Bitboards[0]|Bitboards[1]) > 3) {
        int color = position&1;
        iterative(nodes, 120000, color);
        makemove(bestmove, 0);
        game = game + algebraic(bestmove);
        game+=" ";
    }
    if (popcount(Bitboards[7]) > 1) {
        game+="d";
    }
    else if (Bitboards[7]&Bitboards[1]) {
        game+="b";
    }
    else {
        game+="w";
    }
    cout << game << "\n";
    gameoutput << game << "\n";
    suppressoutput = false;
    randomeval = false;
    initializett();
    clearhistory();
    initializeboard();
}
void uci() {
    string ucicommand;
    getline(cin, ucicommand);
    if (ucicommand == "uci") {
        cout << "id name sscg13 chess engine \n" << "id author sscg13 \n" << "uciok\n";
    }
    if (ucicommand == "quit") {
        gameoutput.close();
        exit(0);
    }
    if (ucicommand == "isready") {
        cout << "readyok\n";
    }
    if (ucicommand == "ucinewgame") {
        initializett();
        clearhistory();
        initializeboard();
    }
    if (ucicommand.substr(0, 8) == "makemove") {
        string mov = ucicommand.substr(9, 5);
        int color = position&1;
        int len = generatemoves(color, 0);
        int played = -1;
        for (int i = 0; i < len; i++) {
            if (algebraic(moves[0][i]) == mov) {
                played = i;
            }
        }
        if (played >= 0) {
            makemove(moves[0][played], false);
        }
    }
    if (ucicommand.substr(0, 17) == "position startpos") {
        initializeboard();
        int color = 0;
        string mov = "";
        for (int i = 24; i <= ucicommand.length(); i++) {
            if ((ucicommand[i]==' ') || (i == ucicommand.length())) {
                int len = generatemoves(color, 0);
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
                int len = generatemoves(color, 0);
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
    if (ucicommand == "eval") {
        int color = position&1;
        cout << evaluate(color) << "\n";
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
    if (ucicommand.substr(0, 8) == "autoplay") {
        int sum = 0;
        int add = 1;
        int reader = ucicommand.length()-1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        autoplay(sum);
    }
    if (ucicommand.substr(0, 8) == "generate") {
        int sum = 0;
        int add = 1;
        int reader = ucicommand.length()-1;
        while (ucicommand[reader] != ' ') {
            sum+=((int)(ucicommand[reader]-48))*add;
            add*=10;
            reader--;
        }
        for (int i = 0; i < sum; i++) {
            autoplay(30000);
        }
    }
}
int main() {
    initializeleaperattacks();
    initializemasks();
    initializerankattacks();
    initializeboard();
    initializezobrist();
    initializett();
    srand(time(0));
    gameoutput.open("doublemove data.txt", ofstream::app);
    while (true) {
        uci();
    }
    return 0;
}
