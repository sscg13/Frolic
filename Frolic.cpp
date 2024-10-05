#include "board.cpp"
#include "nnue.cpp"

#include <bit>
#include <chrono>
#include <fstream>
#include <thread>
#include <time.h>
using U64 = uint64_t;
using namespace std;

std::string proto = "none";
std::string uciinfostring =
    "id name Frolic \nid author sscg13 \noption name Threads type spin default "
    "1 "
    "min 1 max 1 \noption name Hash type spin default 32 min 1 max 1024 "
    "\noption name Use NNUE type check default true \noption name EvalFile "
    "type string default <internal> \nuciok\n";

const int maxmaxdepth = 32;
int lmr_reductions[maxmaxdepth][256];
auto start = std::chrono::steady_clock::now();
struct TTentry {
  U64 key;
  int score;
  int depth;
  int age;
  int nodetype;
  int hashmove;
};
struct abinfo {
  int hashmove;
  int eval;
  bool incheck;
};
class Engine {
  Board Bitboards;
  int historytable[2][6][64];
  int TTsize = 1048576;
  std::vector<TTentry> TT;
  bool useNNUE = true;
  NNUE EUNN;
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
  void updatett(int index, int depth, int score, int nodetype, int hashmove);
  void resethistory();
  int movestrength(int mov, int color);
  int quiesce(int alpha, int beta, int color, int depth);
  int alphabeta(int depth, int ply, int alpha, int beta, int color, bool nmp);
  int wdlmodel(int eval);
  int normalize(int eval);
  int iterative(int color);
  void autoplay();

public:
  void startup();
  void datagen(int n, std::string outputfile);
  void uci();
  void xboard();
};
void Engine::initializett() {
  TT.resize(TTsize);
  for (int i = 0; i < TTsize; i++) {
    TT[i].key = (U64)i + 1ULL;
    TT[i].score = 0;
    TT[i].depth = 0;
    TT[i].age = 0;
    TT[i].nodetype = 0;
    TT[i].hashmove = 0;
  }
}
void Engine::updatett(int index, int depth, int score, int nodetype,
                      int hashmove) {
  if (index < TTsize) {
    TT[index].key = Bitboards.zobristhash;
    TT[index].depth = depth;
    TT[index].age = Bitboards.gamelength;
    TT[index].hashmove = hashmove;
    TT[index].nodetype = nodetype;
    TT[index].score = score;
  }
}
void Engine::resethistory() {
  for (int i = 0; i < 6; i++) {
    for (int j = 0; j < 64; j++) {
      historytable[0][i][j] = 0;
      historytable[1][i][j] = 0;
    }
  }
  for (int i = 0; i < maxmaxdepth; i++) {
    for (int j = 0; j < maxmaxdepth + 1; j++) {
      pvtable[i][j] = 0;
    }
  }
}
void Engine::startup() {
  initializett();
  resethistory();
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
int Engine::movestrength(int mov, int color) {
  int to = (mov >> 6) & 63;
  int piece = (mov >> 13) & 7;
  int captured = (mov >> 17) & 7;
  int promoted = (mov >> 21) & 3;
  return 10000 * captured + 12000 * promoted + 10000 - 1000 * piece +
         historytable[color][piece - 2][to];
}
int Engine::quiesce(int alpha, int beta, int color, int depth) {
  int score = useNNUE ? EUNN.evaluate(color) : Bitboards.evaluate(color);
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
           movestrength(Bitboards.moves[maxdepth + depth][j], color) >
               movestrength(Bitboards.moves[maxdepth + depth][j - 1], color) &&
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
      if (useNNUE) {
        EUNN.forwardaccumulators(mov);
      }
      score = -quiesce(-beta, -alpha, color ^ 1, depth + 1);
      Bitboards.unmakemove(mov);
      if (useNNUE) {
        EUNN.backwardaccumulators(mov);
      }
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
  int ttdepth = TT[index].depth;
  int ttage = std::max(Bitboards.gamelength - TT[index].age, 0);
  bool update = (depth >= (ttdepth - ttage / 3));
  bool isPV = (beta - alpha > 1);
  int staticeval = useNNUE ? EUNN.evaluate(color) : Bitboards.evaluate(color);
  bool incheck = (Bitboards.checkers(color) != 0ULL);
  searchstack[ply].incheck = incheck;
  searchstack[ply].eval = staticeval;
  bool improving = false;
  if (ply > 1) {
    improving = (staticeval > searchstack[ply - 2].eval);
  }
  if (TT[index].key == Bitboards.zobristhash) {
    score = TT[index].score;
    ttmove = TT[index].hashmove;
    int nodetype = TT[index].nodetype;
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
      movescore[i] = movestrength(mov, color);
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
      if (useNNUE) {
        EUNN.forwardaccumulators(mov);
      }
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
      if (useNNUE) {
        EUNN.backwardaccumulators(mov);
      }
      if (score > bestscore) {
        if (score > alpha) {
          if (score >= beta) {
            if (update && !stopsearch && abs(score) < 29000) {
              updatett(index, depth, score, 1, mov);
            }
            if ((((mov >> 16) & 1) == 0) && (killers[ply][0] != mov)) {
              killers[ply][1] = killers[ply][0];
              killers[ply][0] = mov;
            }
            int target = (mov >> 6) & 63;
            int piece = (mov >> 13) & 7;
            historytable[color][piece - 2][target] +=
                (depth * depth * depth -
                 (depth * depth * depth *
                  historytable[color][piece - 2][target]) /
                     27000);
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
    updatett(index, depth, bestscore, 2 + allnode,
             Bitboards.moves[ply][bestmove1]);
  }
  return bestscore;
}
int Engine::iterative(int color) {
  Bitboards.nodecount = 0;
  stopsearch = false;
  start = std::chrono::steady_clock::now();
  int score = useNNUE ? EUNN.evaluate(color) : Bitboards.evaluate(color);
  int returnedscore = score;
  int depth = 1;
  int bestmove1 = 0;
  resethistory();
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
          std::cout << "\n";
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
          std::cout << "\n";
        }
      }
      if (proto == "xboard") {
        std::cout << depth << " " << score << " " << timetaken.count() / 10
                  << " " << Bitboards.nodecount << " ";
        for (int i = 1; i < pvtable[0][0]; i++) {
          std::cout << algebraic(pvtable[0][i]) << " ";
        }
        std::cout << "\n";
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
    std::cout << "info nodes " << Bitboards.nodecount << " nps " << nps << "\n";
  }
  if (proto == "uci") {
    std::cout << "bestmove " << algebraic(bestmove1) << "\n";
  }
  if (proto == "xboard") {
    std::cout << "move " << algebraic(bestmove1) << "\n";
    Bitboards.makemove(bestmove1, 0);
    if (useNNUE) {
      EUNN.forwardaccumulators(bestmove1);
    }
  }
  bestmove = bestmove1;
  return returnedscore;
}
void Engine::autoplay() {
  suppressoutput = true;
  initializett();
  resethistory();
  int seed = mt() % 40320;
  Bitboards.parseFEN(get8294400FEN(seed, seed));
  std::string game = "";
  std::string result = "";
  for (int i = 0; i < 8; i++) {
    int num_moves = Bitboards.generatemoves(i & 1, 0, 0);
    if (num_moves == 0) {
      suppressoutput = false;
      initializett();
      resethistory();
      seed = mt() % 40320;
      Bitboards.parseFEN(get8294400FEN(seed, seed));
      return;
    }
    bool sound = false;
    int rand_move = 0;
    while (!sound) {
      int rand_move = mt() % num_moves;
      sound = (Bitboards.checkers(i & 1) != 0ULL ||
               Bitboards.see_exceeds(Bitboards.moves[0][rand_move], i & 1, 0));
    }
    Bitboards.makemove(Bitboards.moves[0][rand_move], 0);
    game += algebraic(Bitboards.moves[0][rand_move]);
    game += " ";
  }
  if (useNNUE) {
    EUNN.initializennue(Bitboards.Bitboards);
  }
  if (Bitboards.generatemoves(0, 0, 0) == 0) {
    suppressoutput = false;
    initializett();
    resethistory();
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
    if (useNNUE && bestmove > 0) {
      EUNN.forwardaccumulators(bestmove);
    }
  }
  for (int i = 0; i < maxmove; i++) {
    dataoutput << fens[i] << " | " << scores[i] << " | " << result << "\n";
  }
  suppressoutput = false;
  initializett();
  resethistory();
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
  if (ucicommand.substr(0, 23) == "setoption name EvalFile") {
    std::string nnuefile = ucicommand.substr(30, ucicommand.length() - 30);

    if (nnuefile == "<empty>") {
      useNNUE = false;
    } else if (nnuefile != "<internal>") {
      EUNN.readnnuefile(nnuefile);
      EUNN.initializennue(Bitboards.Bitboards);
      std::cout << "info string using nnue file " << nnuefile << "\n";
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
      if (useNNUE) {
        EUNN.forwardaccumulators(Bitboards.moves[0][played]);
      }
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
