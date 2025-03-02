#include "board.cpp"
#include <string>
#include <algorithm>
int pieceoffsets[12][66];
int coloroffset;

int offset = 0;
U64 emptyboardattacks(int piece, int square) {
    if (piece == 0 || piece == 6) {
        if (square < 8 || square > 55) {
            return 0ULL;
        }
        else {
            return PawnAttacks[piece/6][square];
        }
    }
    if (piece == 1 || piece == 7) {
        return KnightAttacks[square];
    }
    if (piece == 5 || piece == 11) {
        return KingAttacks[square];
    }
    U64 BishopAttacks = DiagAttacks((1ULL << square), square) | AntiAttacks((1ULL << square), square);
    U64 RookAttacks = GetRankAttacks((1ULL << square), square) | FileAttacks((1ULL << square), square);
    if (piece == 2 || piece == 8) {
        return BishopAttacks;
    }
    if (piece == 3 || piece == 9) {
        return RookAttacks;
    }
    if (piece == 4 || piece == 10) {
        return BishopAttacks | RookAttacks;
    }
    return 0ULL;
}
U64 nonemptyboardattacks(int piece, int square, U64 occupancy) {
    if (piece == 0 || piece == 6) {
        if (square < 8 || square > 55) {
            return 0ULL;
        }
        else {
            return PawnAttacks[piece/6][square] & occupancy;
        }
    }
    if (piece == 1 || piece == 7) {
        return KnightAttacks[square] & occupancy;
    }
    if (piece == 5 || piece == 11) {
        return KingAttacks[square] & occupancy;
    }
    U64 BishopAttacks = DiagAttacks(occupancy, square) | AntiAttacks(occupancy, square);
    U64 RookAttacks = GetRankAttacks(occupancy, square) | FileAttacks(occupancy, square);
    if (piece == 2 || piece == 8) {
        return BishopAttacks & occupancy;
    }
    if (piece == 3 || piece == 9) {
        return RookAttacks & occupancy;
    }
    if (piece == 4 || piece == 10) {
        return (BishopAttacks | RookAttacks) & occupancy;
    }
    return 0ULL;
}
void filloffsets() {
    int pieceoffset = 0;
    for (int piece = 0; piece < 12; piece++) {
        pieceoffsets[piece][65] = 2*pieceoffset;
        int squareoffset = 0;
        for (int from = 0; from < 64; from++) {
            pieceoffsets[piece][from] = squareoffset;
            U64 attacks = emptyboardattacks(piece, from);
            squareoffset += __builtin_popcountll(attacks);
        }
        pieceoffsets[piece][64] = squareoffset;
        pieceoffset += squareoffset;
    }
}
int threatindex(int attacker, int from, int to, bool enemy) {
    /*std::cout << pieceoffsets[attacker][65];
    std::cout << "+" << enemy*pieceoffsets[attacker][64];
    std::cout << "+" << pieceoffsets[attacker][from];
    std::cout << "+" << __builtin_popcountll(((1ULL << to)-1) & emptyboardattacks(attacker, from));
    std::cout << "+768=";*/
    return pieceoffsets[attacker][65]
        + enemy*pieceoffsets[attacker][64]
        + pieceoffsets[attacker][from]
        + __builtin_popcountll(((1ULL << to)-1) & emptyboardattacks(attacker, from))
        + 768;
}
int main(int argc, char *argv[]) {
    initializeleaperattacks();
    initializemasks();
    initializerankattacks();
    filloffsets();
    Board Bitboards;
    std::string fen = std::string(argv[1]);
    Bitboards.parseFEN(fen);
    int whitekingsquare = __builtin_ctzll(Bitboards.Bitboards[0] & Bitboards.Bitboards[7]);
    int mirror = ((whitekingsquare & 7) >= 4) ? 7 : 0;
    U64 occupancy = Bitboards.Bitboards[0] | Bitboards.Bitboards[1];
    std::vector<int> indices;
    while (occupancy) {
        int from = __builtin_ctzll(occupancy);
        int color = Bitboards.color(from);
        int piece = Bitboards.piece(from);
        U64 attacks = nonemptyboardattacks(6*color+piece, from, Bitboards.Bitboards[0] | Bitboards.Bitboards[1]);
        while (attacks) {
            int to = __builtin_ctzll(attacks);
            int enemy = (Bitboards.color(to) != color);
            int index = threatindex(6*color+piece, mirror^from, mirror^to, enemy);
            indices.push_back(index);
            std::cout << coordinate(from) << coordinate(to) << " index: " << index << std::endl;
            attacks ^= (1ULL << to);
        }
        occupancy ^= (1ULL << from);
    }
    std::sort(indices.begin(), indices.end());
    for (auto index : indices) {
        std::cout << index << ", ";
    }
    return 0;
}