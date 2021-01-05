
/*
 * uct.h
 */
 
int uct_init_all(void);
void uct_set_memory(int max_memory);
void uct_init(int size, int num_threads, int max_playouts);
void uct_init_komi(int komi);
void uct_free_all();
void uct_makeamove(sqr_t s, int c);
void uct_takebackmove(void);
sqr_t uct_compmove(int color, int handicap, int rules, unsigned int time_left, unsigned int stones_left, int show_lookahead, int using_mpi, int ranks_per_group, float *win_rate);
void po_test_generate(int handicap, int color);
int uct_get_playout_count(void);
void uct_show_prior(int, int, int, int);
