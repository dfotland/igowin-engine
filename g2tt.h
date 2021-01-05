
/* transposition table */
#define TTUNUSED 0
#define EXACT 1
#define LOWERBOUND 2
#define UPPERBOUND 3

void tt_init();
int tt_lookup(__int64 hashindex, int *flag, float *depth, int *value, sqr_t *m);
void addentry(__int64 hashindex, int flag, float depth, int value, sqr_t m);
int te_lookup(__int64 hashindex, sqr_t s, int *value );
void te_addentry(__int64 hashindex, int value, sqr_t m);

