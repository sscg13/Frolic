#include "board.cpp"
#include "history.cpp"
#include "nnue.cpp"
#include "tt.cpp"
#include <bit>
#include <chrono>
#include <fstream>
#include <thread>
#include <time.h>
using U64 = uint64_t;
using namespace std;

std::string proto = "none";
//clang-format off
std::string uciinfostring =
    "id name Frolic\n"
    "id author sscg13\n"
    "option name Threads type spin default 1 min 1 max 1\n"
    "option name Hash type spin default 32 min 1 max 1024\n"
    "uciok\n";
//clang-format on
const int maxmaxdepth = 32;
int lmr_reductions[maxmaxdepth][256];
auto start = std::chrono::steady_clock::now();
struct abinfo {
  int hashmove;
  int eval;
  bool incheck;
};
class Engine {
  Board Bitboards;
  int TTsize = 2097152;
  std::vector<TTentry> TT;
  NNUE EUNN;
  History Histories;
  int killers[maxmaxdepth][2];
  bool gosent = false;
  bool stopsearch = false;
  bool suppressoutput = false;
  int maxdepth = maxmaxdepth;
  abinfo searchstack[64];
  int pvtable[maxmaxdepth + 1][maxmaxdepth + 1];
  int bestmove = 0;
  int movetime = 0;
  int softnodelimit = 0;
  int hardnodelimit = 0;
  int softtimelimit = 0;
  int hardtimelimit = 0;
  std::random_device rd;
  std::mt19937 mt;
  std::ofstream dataoutput;
  void initializett();
  void resetauxdata();
  int movestrength(int mov, int color);
  int quiesce(int alpha, int beta, int color, int depth);
  int alphabeta(int depth, int ply, int alpha, int beta, int color, bool nmp);
  int wdlmodel(int eval);
  int normalize(int eval);
  int iterative(int color);
  void autoplay();

public:
  void startup();
  void bench();
  void datagen(int n, std::string outputfile);
  void uci();
  void xboard();
};
void Engine::initializett() {
  TT.resize(TTsize);
  for (int i = 0; i < TTsize; i++) {
    TT[i].key = (U64)i + 1ULL;
    TT[i].data = 0;
  }
}
void Engine::resetauxdata() {
  for (int i = 0; i < maxmaxdepth; i++) {
    for (int j = 0; j < maxmaxdepth + 1; j++) {
      pvtable[i][j] = 0;
    }
  }
  Histories.reset();
}
void Engine::startup() {
  initializett();
  resetauxdata();
  Bitboards.initialize();
  EUNN.loaddefaultnet();
  EUNN.initializennue(Bitboards.Bitboards);
  mt.seed(rd());
}
void initializelmr() {
  for (int i = 0; i < maxmaxdepth; i++) {
    for (int j = 0; j < 256; j++) {
      lmr_reductions[i][j] =
          (i == 0 || j == 0) ? 0 : floor(0.53 + log(i) * log(j) / 2.44);
    }
  }
}
int Engine::quiesce(int alpha, int beta, int color, int depth) {
  int score = EUNN.evaluate(color);
  int bestscore = -30000;
  int movcount;
  if (depth > 4) {
    return score;
  }
  bool incheck = Bitboards.checkers(color);
  if (incheck) {
    movcount = Bitboards.generatemoves(color, 0, maxdepth + depth);
    if (movcount == 0) {
      return -27000;
    }
  } else {
    bestscore = score;
    if (alpha < score) {
      alpha = score;
    }
    if (score >= beta) {
      return score;
    }
    movcount = Bitboards.generatemoves(color, 1, maxdepth + depth);
  }
  if (depth < 4) {
    for (int i = 0; i < movcount - 1; i++) {
      for (int j = i + 1;
           Histories.movescore(Bitboards.moves[maxdepth + depth][j]) >
               Histories.movescore(Bitboards.moves[maxdepth + depth][j - 1]) &&
           j > 0;
           j--) {
        std::swap(Bitboards.moves[maxdepth + depth][j],
                  Bitboards.moves[maxdepth + depth][j - 1]);
      }
    }
  }
  for (int i = 0; i < movcount; i++) {
    int mov = Bitboards.moves[maxdepth + depth][i];
    bool good = (incheck || Bitboards.see_exceeds(mov, color, 0));
    if (good) {
      Bitboards.makemove(mov, 1);
      EUNN.forwardaccumulators(mov);
      score = -quiesce(-beta, -alpha, color ^ 1, depth + 1);
      Bitboards.unmakemove(mov);
      EUNN.backwardaccumulators(mov);
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
  }
  return bestscore;
}
int Engine::alphabeta(int depth, int ply, int alpha, int beta, int color,
                      bool nmp) {
  pvtable[ply][0] = ply + 1;
  if (Bitboards.repetitions() > 1) {
    return 0;
  }
  if (Bitboards.halfmovecount() >= 100) {
    return 0;
  }
  if (Bitboards.insufficientmaterial()) {
    return 0;
  }
  if (depth <= 0 || ply >= maxdepth) {
    return quiesce(alpha, beta, color, 0);
  }
  int score = -30000;
  int bestscore = -30000;
  int allnode = 0;
  int movcount;
  int index = Bitboards.zobristhash % TTsize;
  int ttmove = 0;
  int bestmove1 = -1;
  int ttdepth = TT[index].depth();
  int ttage = TT[index].age(Bitboards.gamelength);
  bool update = (depth >= (ttdepth - ttage / 2));
  bool isPV = (beta - alpha > 1);
  int staticeval = EUNN.evaluate(color);
  bool incheck = (Bitboards.checkers(color) != 0ULL);
  searchstack[ply].incheck = incheck;
  searchstack[ply].eval = staticeval;
  bool improving = false;
  if (ply > 1) {
    improving = (staticeval > searchstack[ply - 2].eval);
  }
  if (!isPV) {
    alpha = std::max(alpha, -28000 + ply);
    beta  = std::min(beta,  28000 - ply - 1);
    if (alpha >= beta) {
      return alpha;
    }
  }
  if (TT[index].key == Bitboards.zobristhash) {
    score = TT[index].score(ply);
    ttmove = TT[index].hashmove();
    int nodetype = TT[index].nodetype();
    if (ttdepth >= depth) {
      if (!isPV && Bitboards.repetitions() == 0) {
        if (nodetype == 3) {
          return score;
        }
        if ((nodetype & 1) && (score >= beta)) {
          return score;
        }
        if ((nodetype & 2) && (score <= alpha)) {
          return score;
        }
      }
    } else {
      int margin = 40 + 60 * (depth - ttdepth);
      if ((nodetype & 1) && (score - margin >= beta) && (abs(beta) < 27000) &&
          !incheck && (ply > 0)) {
        return (score + beta) / 2;
      }
    }
    staticeval = score;
  }
  int margin = 40 + 60 * (depth);
  if (ply > 0 && score == -30000) {
    if (staticeval - margin >= beta && (abs(beta) < 27000) && !incheck) {
      return (staticeval + beta) / 2;
    }
  }
  movcount = Bitboards.generatemoves(color, 0, ply);
  if (movcount == 0) {
    if (Bitboards.checkers(color)) {
      return -1 * (28000 - ply);
    } else {
      return 0;
    }
  }
  if ((!incheck && Bitboards.gamephase[color] > 0) && (depth > 1 && nmp) &&
      (staticeval > beta)) {
    Bitboards.makenullmove();
    score = -alphabeta(depth - 2 - depth / 3, ply + 1, -beta, 1 - beta,
                       color ^ 1, false);
    Bitboards.unmakenullmove();
    if (score >= beta) {
      return beta;
    }
  }
  int movescore[256];
  for (int i = 0; i < movcount; i++) {
    int mov = Bitboards.moves[ply][i];
    if (mov == ttmove) {
      movescore[i] = (1 << 20);
    } else {
      movescore[i] = Histories.movescore(mov);
    }
    if (mov == killers[ply][0]) {
      movescore[i] += 20000;
    }
    if (mov == killers[ply][1]) {
      movescore[i] += 10000;
    }
    int j = i;
    while (j > 0 && movescore[j] > movescore[j - 1]) {
      std::swap(Bitboards.moves[ply][j], Bitboards.moves[ply][j - 1]);
      std::swap(movescore[j], movescore[j - 1]);
      j--;
    }
  }
  for (int i = 0; i < movcount; i++) {
    bool nullwindow = (i > 0);
    int mov = Bitboards.moves[ply][i];
    int r = (movescore[i] < 30000)
                ? std::min(depth - 1, lmr_reductions[depth][i])
                : 0;
    r = std::max(0, r - (incheck || isPV) - movescore[i] / 16384);
    int e = (movcount == 1) ? 1 : 0;
    // bool prune = ((beta-alpha < 2) && (depth < 5) && (i > 6+4*depth) &&
    // movescore[depth][i] < 1000);
    if (!stopsearch) {
      Bitboards.makemove(mov, true);
      EUNN.forwardaccumulators(mov);
      if (nullwindow) {
        score = -alphabeta(depth - 1 - r, ply + 1, -alpha - 1, -alpha,
                           color ^ 1, true);
        if (score > alpha && r > 0) {
          score = -alphabeta(depth - 1, ply + 1, -alpha - 1, -alpha, color ^ 1,
                             true);
        }
        if (score > alpha && score < beta) {
          score =
              -alphabeta(depth - 1, ply + 1, -beta, -alpha, color ^ 1, true);
        }
      } else {
        score =
            -alphabeta(depth - 1 + e, ply + 1, -beta, -alpha, color ^ 1, true);
      }
      Bitboards.unmakemove(mov);
      EUNN.backwardaccumulators(mov);
      if (score > bestscore) {
        if (score > alpha) {
          if (score >= beta) {
            if (update && !stopsearch && abs(score) < 29000) {
              TT[index].update(Bitboards.zobristhash, Bitboards.gamelength,
                               depth, ply, score, 1, mov);
            }
            if ((((mov >> 16) & 1) == 0) && (killers[ply][0] != mov)) {
              killers[ply][1] = killers[ply][0];
              killers[ply][0] = mov;
            }
            if ((mov >> 16) & 1) {
              Histories.updatenoisyhistory(mov, depth * depth);
            } else {
              Histories.updatequiethistory(mov, depth * depth * depth);
            }
            return score;
          }
          alpha = score;
          allnode = 1;
        }
        if (ply == 0) {
          bestmove = mov;
        }
        pvtable[ply][ply + 1] = mov;
        pvtable[ply][0] = pvtable[ply + 1][0] ? pvtable[ply + 1][0] : ply + 2;
        for (int j = ply + 2; j < pvtable[ply][0]; j++) {
          pvtable[ply][j] = pvtable[ply + 1][j];
        }
        bestmove1 = i;
        bestscore = score;
      }
      if (Bitboards.nodecount > hardnodelimit && hardnodelimit > 0) {
        stopsearch = true;
      }
      if ((Bitboards.nodecount & 1023) == 0) {
        auto now = std::chrono::steady_clock::now();
        auto timetaken =
            std::chrono::duration_cast<std::chrono::milliseconds>(now - start);
        if (timetaken.count() > hardtimelimit && hardtimelimit > 0) {
          stopsearch = true;
        }
      }
    }
  }
  if (update && !stopsearch) {
    TT[index].update(Bitboards.zobristhash, Bitboards.gamelength, depth, ply,
                     bestscore, 2 + allnode, Bitboards.moves[ply][bestmove1]);
  }
  return bestscore;
}
int Engine::iterative(int color) {
  Bitboards.nodecount = 0;
  stopsearch = false;
  start = std::chrono::steady_clock::now();
  int score = EUNN.evaluate(color);
  int returnedscore = score;
  int depth = 1;
  int bestmove1 = 0;
  resetauxdata();
  while (!stopsearch) {
    bestmove = -1;
    int delta = 30;
    int alpha = score - delta;
    int beta = score + delta;
    bool fail = true;
    while (fail) {
      int score1 = alphabeta(depth, 0, alpha, beta, color, false);
      if (score1 >= beta) {
        beta += delta;
        delta += delta;
      } else if (score1 <= alpha) {
        alpha -= delta;
        delta += delta;
      } else {
        score = score1;
        fail = false;
      }
    }
    auto now = std::chrono::steady_clock::now();
    auto timetaken =
        std::chrono::duration_cast<std::chrono::milliseconds>(now - start);
    if ((Bitboards.nodecount < hardnodelimit || hardnodelimit <= 0) &&
        (timetaken.count() < hardtimelimit || hardtimelimit <= 0) &&
        depth < maxdepth && bestmove >= 0) {
      returnedscore = score;
      if (proto == "uci" && !suppressoutput) {
        if (abs(score) <= 27000) {
          std::cout << "info depth " << depth << " nodes "
                    << Bitboards.nodecount << " time " << timetaken.count()
                    << " score cp " << score << " pv ";
          for (int i = 1; i < pvtable[0][0]; i++) {
            std::cout << algebraic(pvtable[0][i]) << " ";
          }
          std::cout << std::endl;
        } else {
          int matescore;
          if (score > 0) {
            matescore = 1 + (28000 - score) / 2;
          } else {
            matescore = (-28000 - score) / 2;
          }
          std::cout << "info depth " << depth << " nodes "
                    << Bitboards.nodecount << " time " << timetaken.count()
                    << " score mate " << matescore << " pv ";
          for (int i = 1; i < pvtable[0][0]; i++) {
            std::cout << algebraic(pvtable[0][i]) << " ";
          }
          std::cout << std::endl;
        }
      }
      if (proto == "xboard") {
        std::cout << depth << " " << score << " " << timetaken.count() / 10
                  << " " << Bitboards.nodecount << " ";
        for (int i = 1; i < pvtable[0][0]; i++) {
          std::cout << algebraic(pvtable[0][i]) << " ";
        }
        std::cout << std::endl;
      }
      depth++;
      if (depth == maxdepth) {
        stopsearch = true;
      }
      bestmove1 = bestmove;
    } else {
      stopsearch = true;
    }
    if ((timetaken.count() > softtimelimit && softtimelimit > 0) ||
        (Bitboards.nodecount > softnodelimit && softnodelimit > 0)) {
      stopsearch = true;
    }
  }
  auto now = std::chrono::steady_clock::now();
  auto timetaken =
      std::chrono::duration_cast<std::chrono::milliseconds>(now - start);
  if (proto == "uci") {
    int nps = 1000 * (Bitboards.nodecount / std::max(1LL, timetaken.count()));
    std::cout << "info nodes " << Bitboards.nodecount << " nps " << nps
              << std::endl;
  }
  if (proto == "uci") {
    std::cout << "bestmove " << algebraic(bestmove1) << std::endl;
  }
  if (proto == "xboard") {
    std::cout << "move " << algebraic(bestmove1) << std::endl;
    Bitboards.makemove(bestmove1, 0);
    EUNN.forwardaccumulators(bestmove1);
  }
  bestmove = bestmove1;
  return returnedscore;
}
void Engine::autoplay() {
  suppressoutput = true;
  initializett();
  resetauxdata();
  Bitboards.initialize();
  std::string game = "";
  std::string result = "";
  for (int i = 0; i < 12; i++) {
    int num_moves = Bitboards.generatemoves(i & 1, 0, 0);
    if (num_moves == 0) {
      suppressoutput = false;
      initializett();
      resetauxdata();
      Bitboards.initialize();
      return;
    }
    bool sound = false;
    int qsearcheval = quiesce(-29000, 29000, i & 1, 0);
    int rand_move = 0;
    int tried = 0;
    while (!sound) {
      rand_move = mt() % num_moves;
      Bitboards.makemove(Bitboards.moves[0][rand_move], 1);
      if (-quiesce(-29000, 29000, 1 - (i & 1), 0) >=
          qsearcheval - 100 - tried * tried) {
        sound = true;
      } else {
        Bitboards.unmakemove(Bitboards.moves[0][rand_move]);
        tried++;
      }
    }
    game += algebraic(Bitboards.moves[0][rand_move]);
    game += " ";
  }
  EUNN.initializennue(Bitboards.Bitboards);
  if (Bitboards.generatemoves(0, 0, 0) == 0) {
    suppressoutput = false;
    initializett();
    resetauxdata();
    Bitboards.initialize();
    return;
  }
  std::string fens[1024];
  int scores[1024];
  int maxmove = 0;
  bool finished = false;
  while (!finished) {
    int color = Bitboards.position & 1;
    int score = iterative(color);
    if ((bestmove > 0) && (((bestmove >> 16) & 1) == 0) &&
        (Bitboards.checkers(color) == 0ULL) && (abs(score) < 27000)) {
      fens[maxmove] = Bitboards.getFEN();
      scores[maxmove] = score * (1 - 2 * color);
      maxmove++;
    }
    if (bestmove == 0) {
      std::cout << "Null best move? mitigating by using proper null move \n";
      Bitboards.makenullmove();
    } else {
      Bitboards.makemove(bestmove, 0);
    }
    if (Bitboards.insufficientmaterial()) {
      finished = true;
      result = "0.5";
    } else if (Bitboards.repetitions() >= 2) {
      finished = true;
      result = "0.5";
    } else if (Bitboards.generatemoves(color ^ 1, 0, 0) == 0) {
      finished = true;
      if (Bitboards.checkers(color ^ 1) == 0ULL) {
        result = "0.5";
      } else {
        if (color == 0) {
          result = "1.0";
        } else {
          result = "0.0";
        }
      }
    } else if (Bitboards.halfmovecount() >= 100) {
      finished = true;
      result = "0.5";
    } else if (Bitboards.gamelength >= 900) {
      finished = true;
      result = "0.5";
    }
    if (bestmove > 0) {
      EUNN.forwardaccumulators(bestmove);
    }
  }
  for (int i = 0; i < maxmove; i++) {
    dataoutput << fens[i] << " | " << scores[i] << " | " << result << "\n";
  }
  suppressoutput = false;
  initializett();
  resetauxdata();
  Bitboards.initialize();
}
void Engine::datagen(int n, std::string outputfile) {
  dataoutput.open(outputfile, std::ofstream::app);
  softnodelimit = 4096;
  hardnodelimit = 65536;
  softtimelimit = 0;
  hardtimelimit = 0;
  for (int i = 0; i < n; i++) {
    autoplay();
    std::cout << i << "\n";
  }
  dataoutput.close();
}
void Engine::bench() {
  std::string benchfens[50] = {
      "r3k2r/2pb1ppp/2pp1q2/p7/1nP1B3/1P2P3/P2N1PPP/R2QK2R w KQkq a6 0 14",
      "4rrk1/2p1b1p1/p1p3q1/4p3/2P2n1p/1P1NR2P/PB3PP1/3R1QK1 b - - 2 24",
      "r3qbrk/6p1/2b2pPp/p3pP1Q/PpPpP2P/3P1B2/2PB3K/R5R1 w - - 16 42",
      "6k1/1R3p2/6p1/2Bp3p/3P2q1/P7/1P2rQ1K/5R2 b - - 4 44",
      "8/8/1p2k1p1/3p3p/1p1P1P1P/1P2PK2/8/8 w - - 3 54",
      "7r/2p3k1/1p1p1qp1/1P1Bp3/p1P2r1P/P7/4R3/Q4RK1 w - - 0 36",
      "r1bq1rk1/pp2b1pp/n1pp1n2/3P1p2/2P1p3/2N1P2N/PP2BPPP/R1BQ1RK1 b - - 2 10",
      "3r3k/2r4p/1p1b3q/p4P2/P2Pp3/1B2P3/3BQ1RP/6K1 w - - 3 87",
      "2r4r/1p4k1/1Pnp4/3Qb1pq/8/4BpPp/5P2/2RR1BK1 w - - 0 42",
      "4q1bk/6b1/7p/p1p4p/PNPpP2P/KN4P1/3Q4/4R3 b - - 0 37",
      "2q3r1/1r2pk2/pp3pp1/2pP3p/P1Pb1BbP/1P4Q1/R3NPP1/4R1K1 w - - 2 34",
      "1r2r2k/1b4q1/pp5p/2pPp1p1/P3Pn2/1P1B1Q1P/2R3P1/4BR1K b - - 1 37",
      "r3kbbr/pp1n1p1P/3ppnp1/q5N1/1P1pP3/P1N1B3/2P1QP2/R3KB1R b KQkq b3 0 17",
      "8/6pk/2b1Rp2/3r4/1R1B2PP/P5K1/8/2r5 b - - 16 42",
      "1r4k1/4ppb1/2n1b1qp/pB4p1/1n1BP1P1/7P/2PNQPK1/3RN3 w - - 8 29",
      "8/p2B4/PkP5/4p1pK/4Pb1p/5P2/8/8 w - - 29 68",
      "3r4/ppq1ppkp/4bnp1/2pN4/2P1P3/1P4P1/PQ3PBP/R4K2 b - - 2 20",
      "5rr1/4n2k/4q2P/P1P2n2/3B1p2/4pP2/2N1P3/1RR1K2Q w - - 1 49",
      "1r5k/2pq2p1/3p3p/p1pP4/4QP2/PP1R3P/6PK/8 w - - 1 51",
      "q5k1/5ppp/1r3bn1/1B6/P1N2P2/BQ2P1P1/5K1P/8 b - - 2 34",
      "r1b2k1r/5n2/p4q2/1ppn1Pp1/3pp1p1/NP2P3/P1PPBK2/1RQN2R1 w - - 0 22",
      "r1bqk2r/pppp1ppp/5n2/4b3/4P3/P1N5/1PP2PPP/R1BQKB1R w KQkq - 0 5",
      "r1bqr1k1/pp1p1ppp/2p5/8/3N1Q2/P2BB3/1PP2PPP/R3K2n b Q - 1 12",
      "r1bq2k1/p4r1p/1pp2pp1/3p4/1P1B3Q/P2B1N2/2P3PP/4R1K1 b - - 2 19",
      "r4qk1/6r1/1p4p1/2ppBbN1/1p5Q/P7/2P3PP/5RK1 w - - 2 25",
      "r7/6k1/1p6/2pp1p2/7Q/8/p1P2K1P/8 w - - 0 32",
      "r3k2r/ppp1pp1p/2nqb1pn/3p4/4P3/2PP4/PP1NBPPP/R2QK1NR w KQkq - 1 5",
      "3r1rk1/1pp1pn1p/p1n1q1p1/3p4/Q3P3/2P5/PP1NBPPP/4RRK1 w - - 0 12",
      "5rk1/1pp1pn1p/p3Brp1/8/1n6/5N2/PP3PPP/2R2RK1 w - - 2 20",
      "8/1p2pk1p/p1p1r1p1/3n4/8/5R2/PP3PPP/4R1K1 b - - 3 27",
      "8/4pk2/1p1r2p1/p1p4p/Pn5P/3R4/1P3PP1/4RK2 w - - 1 33",
      "8/5k2/1pnrp1p1/p1p4p/P6P/4R1PK/1P3P2/4R3 b - - 1 38",
      "8/8/1p1kp1p1/p1pr1n1p/P6P/1R4P1/1P3PK1/1R6 b - - 15 45",
      "8/8/1p1k2p1/p1prp2p/P2n3P/6P1/1P1R1PK1/4R3 b - - 5 49",
      "8/8/1p4p1/p1p2k1p/P2npP1P/4K1P1/1P6/3R4 w - - 6 54",
      "8/8/1p4p1/p1p2k1p/P2n1P1P/4K1P1/1P6/6R1 b - - 6 59",
      "8/5k2/1p4p1/p1pK3p/P2n1P1P/6P1/1P6/4R3 b - - 14 63",
      "8/1R6/1p1K1kp1/p6p/P1p2P1P/6P1/1Pn5/8 w - - 0 67",
      "1rb1rn1k/p3q1bp/2p3p1/2p1p3/2P1P2N/PP1RQNP1/1B3P2/4R1K1 b - - 4 23",
      "4rrk1/pp1n1pp1/q5p1/P1pP4/2n3P1/7P/1P3PB1/R1BQ1RK1 w - - 3 22",
      "r2qr1k1/pb1nbppp/1pn1p3/2ppP3/3P4/2PB1NN1/PP3PPP/R1BQR1K1 w - - 4 12",
      "2r2k2/8/4P1R1/1p6/8/P4K1N/7b/2B5 b - - 0 55",
      "6k1/5pp1/8/2bKP2P/2P5/p4PNb/B7/8 b - - 1 44",
      "2rqr1k1/1p3p1p/p2p2p1/P1nPb3/2B1P3/5P2/1PQ2NPP/R1R4K w - - 3 25",
      "r1b2rk1/p1q1ppbp/6p1/2Q5/8/4BP2/PPP3PP/2KR1B1R b - - 2 14",
      "6r1/5k2/p1b1r2p/1pB1p1p1/1Pp3PP/2P1R1K1/2P2P2/3R4 w - - 1 36",
      "rnbqkb1r/pppppppp/5n2/8/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2",
      "2rr2k1/1p4bp/p1q1p1p1/4Pp1n/2PB4/1PN3P1/P3Q2P/2RR2K1 w - f6 0 20",
      "3br1k1/p1pn3p/1p3n2/5pNq/2P1p3/1PN3PP/P2Q1PB1/4R1K1 w - - 0 23",
      "2r2b2/5p2/5k2/p1r1pP2/P2pB3/1P3P2/K1P3R1/7R w - - 23 93"};
  suppressoutput = true;
  maxdepth = 10;
  auto commence = std::chrono::steady_clock::now();
  int nodes = 0;
  softnodelimit = 0;
  hardnodelimit = 0;
  softtimelimit = 0;
  hardtimelimit = 0;
  for (int i = 0; i < 50; i++) {
    startup();
    Bitboards.parseFEN(benchfens[i]);
    EUNN.initializennue(Bitboards.Bitboards);
    int color = Bitboards.position & 1;
    iterative(color);
    nodes += Bitboards.nodecount;
  }
  auto conclude = std::chrono::steady_clock::now();
  int timetaken =
      std::chrono::duration_cast<std::chrono::milliseconds>(conclude - commence)
          .count();
  int nps = 1000 * (nodes / timetaken);
  std::cout << nodes << " nodes " << nps << " nps\n";
}
void Engine::uci() {
  std::string ucicommand;
  getline(std::cin, ucicommand);
  if (ucicommand == "uci") {
    std::cout << uciinfostring;
  }
  if (ucicommand == "quit") {
    exit(0);
  }
  if (ucicommand == "isready") {
    std::cout << "readyok\n";
  }
  if (ucicommand == "ucinewgame") {
    initializett();
    Bitboards.initialize();
    EUNN.initializennue(Bitboards.Bitboards);
  }
  if (ucicommand.substr(0, 17) == "position startpos") {
    Bitboards.initialize();
    int color = 0;
    string mov = "";
    for (int i = 24; i <= ucicommand.length(); i++) {
      if ((ucicommand[i] == ' ') || (i == ucicommand.length())) {
        int len = Bitboards.generatemoves(color, 0, 0);
        int played = -1;
        for (int j = 0; j < len; j++) {
          if (algebraic(Bitboards.moves[0][j]) == mov) {
            played = j;
          }
        }
        if (played >= 0) {
          Bitboards.makemove(Bitboards.moves[0][played], false);
          color ^= 1;
        }
        mov = "";
      } else {
        mov += ucicommand[i];
      }
    }
    EUNN.initializennue(Bitboards.Bitboards);
  }
  if (ucicommand.substr(0, 12) == "position fen") {
    int reader = 13;
    while (ucicommand[reader] != 'm' && reader < ucicommand.length()) {
      reader++;
    }
    std::string fen = ucicommand.substr(13, reader - 12);
    Bitboards.parseFEN(fen);
    int color = Bitboards.position & 1;
    std::string mov = "";
    for (int i = reader + 6; i <= ucicommand.length(); i++) {
      if ((ucicommand[i] == ' ') || (i == ucicommand.length())) {
        int len = Bitboards.generatemoves(color, 0, 0);
        int played = -1;
        for (int j = 0; j < len; j++) {
          if (algebraic(Bitboards.moves[0][j]) == mov) {
            played = j;
          }
        }
        if (played >= 0) {
          Bitboards.makemove(Bitboards.moves[0][played], false);
          color ^= 1;
        }
        mov = "";
      } else {
        mov += ucicommand[i];
      }
    }
    EUNN.initializennue(Bitboards.Bitboards);
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
      sum += ((int)(ucicommand[reader] - 48)) * add;
      add *= 10;
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
      sum += ((int)(ucicommand[reader] - 48)) * add;
      add *= 10;
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
    if (reader < ucicommand.length() - 1) {
      sum = 0;
      add = 1;
      while (ucicommand[reader] != ' ') {
        sum += ((int)(ucicommand[reader] - 48)) * add;
        add *= 10;
        reader--;
      }
      winc = sum;
      reader = ucicommand.length() - 1;
      while (ucicommand[reader] == ' ') {
        reader--;
      }
      sum = 0;
      add = 1;
      while (ucicommand[reader] != ' ') {
        sum += ((int)(ucicommand[reader] - 48)) * add;
        add *= 10;
        reader--;
      }
      binc = sum;
    }
    int color = Bitboards.position & 1;
    softnodelimit = 0;
    hardnodelimit = 0;
    if (color == 0) {
      softtimelimit = wtime / 40 + winc / 3;
      hardtimelimit = min(wtime / 2, wtime / 10 + winc);
    } else {
      softtimelimit = btime / 40 + binc / 3;
      hardtimelimit = min(btime / 2, btime / 10 + binc);
    }
    iterative(color);
  }
  if (ucicommand.substr(0, 11) == "go movetime") {
    int sum = 0;
    int add = 1;
    int reader = ucicommand.length() - 1;
    while (ucicommand[reader] != ' ') {
      sum += ((int)(ucicommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    int color = Bitboards.position & 1;
    softnodelimit = 0;
    hardnodelimit = 0;
    softtimelimit = sum;
    hardtimelimit = sum;
    iterative(color);
  }
  if (ucicommand.substr(0, 8) == "go nodes") {
    int sum = 0;
    int add = 1;
    int reader = ucicommand.length() - 1;
    while (ucicommand[reader] != ' ') {
      sum += ((int)(ucicommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    int color = Bitboards.position & 1;
    softnodelimit = sum;
    hardnodelimit = sum;
    softtimelimit = 0;
    hardtimelimit = 0;
    iterative(color);
  }
  if (ucicommand.substr(0, 11) == "go infinite") {
    int color = Bitboards.position & 1;
    softnodelimit = 0;
    hardnodelimit = 0;
    softtimelimit = 0;
    hardtimelimit = 0;
    iterative(color);
  }
  if (ucicommand.substr(0, 8) == "go perft") {
    start = std::chrono::steady_clock::now();
    int color = Bitboards.position & 1;
    int sum = 0;
    int add = 1;
    int reader = ucicommand.length() - 1;
    while (ucicommand[reader] != ' ') {
      sum += ((int)(ucicommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    Bitboards.perft(sum, sum, color);
  }
  if (ucicommand.substr(0, 9) == "go sperft") {
    start = std::chrono::steady_clock::now();
    int color = Bitboards.position & 1;
    int sum = 0;
    int add = 1;
    int reader = ucicommand.length() - 1;
    while (ucicommand[reader] != ' ') {
      sum += ((int)(ucicommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    Bitboards.perftnobulk(sum, sum, color);
  }
  if (ucicommand.substr(0, 14) == "setoption name") {
    int reader = 15;
    std::string option = "";
    while (ucicommand[reader] != ' ') {
      option += ucicommand[reader];
      reader++;
    }
    if (option == "Hash") {
      reader = ucicommand.length() - 1;
      int sum = 0;
      int add = 1;
      while (ucicommand[reader] != ' ') {
        sum += ((int)(ucicommand[reader] - 48)) * add;
        add *= 10;
        reader--;
      }
      if (sum <= 1024) {
        int oldTTsize = TTsize;
        TTsize = 65536 * sum;
        TT.resize(TTsize);
        TT.shrink_to_fit();
      }
    }
  }
  if (ucicommand.substr(0, 3) == "see") {
    std::string mov = ucicommand.substr(4, ucicommand.length() - 4);
    int color = Bitboards.position & 1;
    int movcount = Bitboards.generatemoves(color, 0, 0);
    int internal = 0;
    for (int i = 0; i < movcount; i++) {
      if (algebraic(Bitboards.moves[0][i]) == mov) {
        internal = Bitboards.moves[0][i];
      }
    }
    std::cout << algebraic(internal) << " "
              << Bitboards.see_exceeds(internal, color, 0) << "\n";
  }
}
void Engine::xboard() {
  std::string xcommand;
  getline(std::cin, xcommand);
  if (xcommand.substr(0, 8) == "protover") {
    cout << "feature ping=1 setboard=1 analyze=0 sigint=0 sigterm=0 "
            "myname=\"sscg13 engine\" variants=\"normal\" done=1\n";
  }
  if (xcommand == "new") {
    initializett();
    Bitboards.initialize();
    EUNN.initializennue(Bitboards.Bitboards);
    gosent = false;
  }
  if (xcommand.substr(0, 8) == "setboard") {
    std::string fen = xcommand.substr(9, xcommand.length() - 9);
    Bitboards.parseFEN(fen);
    EUNN.initializennue(Bitboards.Bitboards);
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
      sum += ((int)(xcommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    softtimelimit = sum / 48;
    hardtimelimit = sum / 16;
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
      sum1 += ((int)(xcommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    add = 10000;
    reader = save + 1;
    if (xcommand[save] == ':') {
      while (xcommand[reader] != ' ') {
        sum1 += ((int)(xcommand[reader] - 48)) * add;
        add /= 10;
        reader++;
      }
    }
    add = 1000;
    bool incenti = false;
    reader = xcommand.length() - 1;
    while (xcommand[reader] != ' ') {
      if (xcommand[reader] >= '0') {
        sum2 += ((int)xcommand[reader] - 48) * add;
        add *= 10;
      }
      if (xcommand[reader] == '.') {
        incenti = true;
      }
      reader--;
    }
    if (incenti) {
      sum2 /= 100;
    }
    softtimelimit = sum1 / 40 + sum2 / 3;
    hardtimelimit = sum1 / 10 + sum2;
  }
  if (xcommand.substr(0, 4) == "ping") {
    int sum = 0;
    int add = 1;
    int reader = xcommand.length() - 1;
    while (xcommand[reader] != ' ') {
      sum += ((int)(xcommand[reader] - 48)) * add;
      add *= 10;
      reader--;
    }
    std::cout << "pong " << sum << "\n";
  }
  if ((xcommand.length() == 4) || (xcommand.length() == 5)) {
    int color = Bitboards.position & 1;
    int len = Bitboards.generatemoves(color, 0, 0);
    int played = -1;
    for (int j = 0; j < len; j++) {
      if (algebraic(Bitboards.moves[0][j]) == xcommand) {
        played = j;
      }
    }
    if (played >= 0) {
      Bitboards.makemove(Bitboards.moves[0][played], false);
      EUNN.forwardaccumulators(Bitboards.moves[0][played]);
      if (gosent) {
        int color = Bitboards.position & 1;
        softnodelimit = 0;
        hardnodelimit = 0;
        iterative(color);
      }
    }
  }
  if (xcommand == "go") {
    int color = Bitboards.position & 1;
    softnodelimit = 0;
    hardnodelimit = 0;
    iterative(color);
    gosent = true;
  }
}
int main(int argc, char *argv[]) {
  initializeleaperattacks();
  initializemasks();
  initializerankattacks();
  initializezobrist();
  initializelmr();
  if (argc > 1) {
    if (std::string(argv[1]) == "bench") {
      Engine Frolic;
      Frolic.startup();
      Frolic.bench();
      return 0;
    }
    if (std::string(argv[1]) == "datagen") {
      if (argc < 5) {
        std::cerr << "Proper usage: ./(exe) datagen threads games outputfile";
        return 0;
      }
      int threads = atoi(argv[2]);
      int games = atoi(argv[3]);
      std::cout << "Generating NNUE data with " << threads << " threads x "
                << games << " games:\n";
      std::vector<std::thread> datagenerators(threads);
      std::vector<Engine> Engines(threads);
      for (int i = 0; i < threads; i++) {
        std::string outputfile =
            std::string(argv[4]) + std::to_string(i) + ".txt";
        Engines[i].startup();
        datagenerators[i] =
            std::thread(&Engine::datagen, &Engines[i], games, outputfile);
      }
      for (auto &thread : datagenerators) {
        thread.join();
      }
      return 0;
    }
  }
  Engine Frolic;
  Frolic.startup();
  getline(std::cin, proto);
  if (proto == "uci") {
    std::cout << uciinfostring;
    while (true) {
      Frolic.uci();
    }
  }
  if (proto == "xboard") {
    while (true) {
      Frolic.xboard();
    }
  }
  return 0;
}
