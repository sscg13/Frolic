class History {
  int history[2][6][64];
  const int limit = 27000;

public:
  void reset();
  int movescore(int move);
  void updatehistory(int move, int bonus);
};

void History::reset() {
  for (int i = 0; i < 6; i++) {
    for (int j = 0; j < 64; j++) {
      history[0][i][j] = 0;
      history[1][i][j] = 0;
    }
  }
}

int History::movescore(int move) {
  int color = (move >> 12) & 1;
  int to = (move >> 6) & 63;
  int piece = (move >> 13) & 7;
  int captured = (move >> 17) & 7;
  int promoted = (move >> 21) & 3;
  return 10000 * captured + 12000 * promoted + 10000 - 1000 * piece +
         history[color][piece - 2][to];
}

void History::updatehistory(int move, int bonus) {
  int target = (move >> 6) & 63;
  int piece = (move >> 13) & 7;
  int color = (move >> 12) & 1;
  history[color][piece - 2][target] +=
      (bonus > 0)
          ? (bonus - (bonus * history[color][piece - 2][target]) / limit)
          : bonus;
}