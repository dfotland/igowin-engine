
/*
 * playouts
 *
 * call po_init_all once at program start
 * po_alloc a gboard to track the true board position at the base of uct
 * po_copy that board and po_makemove to get position to the leaf
 * po_playout from there
 */

/* max depth typically seen is less than 30 on 9x9 and 20 on 19x19 */
#define UCT_MAX_DEPTH 60
/* always stop after MAX_PLAYOUTS */
#define UCT_MAX_PLAYOUTS 100000000
/* batch size is adjusted for optimal exploration by different threads before sharing resutls */
#define UCT_PLAYOUT_BATCH 200
#define UCT_PLAYOUT_TIME_CHECK 200
#define UCT_TIMED 1
/* after MIN_PLAYOUTS, look for early exit */
#define UCT_MIN_PLAYOUTS 5000
/* maximum number of allowed children in a UCT interior node */
#define UCT_MAX_CHILDREN (30)
/* maximum number of generated moves in a UCT node */
#define UCT_MAX_MOVES (19*19+1)

#define UCT_MPI_ALLREDUCE 0

/* must be a power of 2 */
#ifdef WIN32
#define UCT_MAX_THREADS 16
#else
#define UCT_MAX_THREADS 1
#endif

#define UCT_SEND_MFGO 0

/* 20 is far stronger at 9x9 go.  50 or 100 is stronger at 19x19 */
#define UCT_SHARED_VISITS 20
/* weaker */
#define UCT_ADD_SHARED 0
/* much weaker */
#define UCT_SEND_ADJUST_RAVE 0

#define UCT_ENABLE_PONDER 0

#define UCT_SEND_NO_EMPTY 1

/* best known UCT without rave is 0.45 */
#define UCT_K (0.45f)
/* without rave, 1.2 FPU is optimal. */
#define UCT_FPU (1.1f)
#define UCT_PO_PER_NODE 2
#define UCT_PO_PER_NODE_9x9 4

#define UCT_PRUNE 1
#define UCT_PRUNE_EDGE 1
#define UCT_LEARN 0

#define UCT_BETA_K (500.f)
#define UCT_RAVE_NEW_BETA 0
#define UCT_RAVE_NEW_BETA_K 0.05f
#define UCT_MFGO_NO_RAVE 0

/* can't get good scaore after two passes with Japanese rules */
#define UCT_TWO_PASS 0

#define UCT_TRIP 1
#define UCT_ADD_BEST 1

#define UCT_PROGRESSIVE_MIN 2
#define UCT_PROGRESSIVE_START 2
#define UCT_PROGRESSIVE_START_MOVES 1
#define UCT_EMPTY_SLOT 1

#define UCT_GC_VISITS 2

/* must fit in short with room for rave prior */
#define UCT_RAVE_MAX_UPDATE 1000
#define UCT_RAVE_PRIOR_CAPTURE 0.2f
#define UCT_RAVE_PRIOR_CLOSE_ENABLE 1
#define UCT_RAVE_PRIOR_CLOSE 0.05f
#define UCT_RAVE_PRIOR_PATTERNS_ANYWHERE 1
#define UCT_RAVE_PRIOR_PATTERN 0.05f
#define UCT_RAVE_PRIOR_SAVE 0.05f
#define UCT_RAVE_PRIOR_CAPTURE_LAST 0.1f
#define UCT_RAVE_PRIOR_SELF_ATARI_ENABLE 0
#define UCT_RAVE_PRIOR_SELF_ATARI -0.05f
#define UCT_RAVE_PRIOR_PASS 0.5f

#define UCT_PRIOR_MFGO 1
#define UCT_SAVE_MFGO_PRIOR 0
#define UCT_PRIOR_MFGO_WEIGHT (0.7f)
#define UCT_PRIOR_MFGO_VISITS 100
#define UCT_PRIOR_MFGO_PLAYS 2000
#define UCT_PRIOR_DELETE_LADDER 0
//#define UCT_PRIOR_NON_MFGO (-0.3f)
#define UCT_PRIOR_NON_MFGO (-0.2f)
#define UCT_PRIOR_KILL_NON_URGENT 1
#define UCT_PRIOR_NON_URGENT (-0.3f)
/* 1% per estimated point */
#define UCT_PRIOR_MFGO_VAL (50.f * 100.f)
/* 5% per oba more points */
#define UCT_PRIOR_MFGO_OBAVAL (0.05f)
#define UCT_PRIOR_MAX 0.95f

#define UCT_ROOT_BIAS_NO_MFGO (-0.01f)

#define UCT_REUSE_VISITS UCT_PRIOR_MFGO_VISITS

/* prior weights - biggest weight plus PRIOR_MFGO_VISITS must be less than 256 to prevent overflow */
#define UCT_RAVE_FPU_PLAYS 10
#define UCT_RAVE_FPU_WINS 5
#define UCT_RAVE_FPU ((float)UCT_RAVE_FPU_WINS / (float)UCT_RAVE_FPU_PLAYS)
#define UCT_RAVE_PRIOR_LOCAL_PLAYS 50
#define UCT_RAVE_PRIOR_CAPTURE_PLAYS 100


#define UCT_GARBAGE_INC_SIZE 200

/* mogo and others say they ignore self */
#define UCT_RAVE_ALL 1
#define UCT_RAVE_PAT_MATCH 0

/* keep liberty bitmap */
#define PO_LIBS 1

#define PO_RANDOM_INIT 1
#define PO_RANDOM_64 1
/* peep still makes it weaker */
#define PO_PEEP 1
#define PO_CAPTURE_LAST 1
#define PO_SAVE_CAPTURE 1
#define PO_SAVE_NO_SELF_ATARI 1
#define PO_CAPTURE 1
#define PO_CAPTURE_STONE 5
#define PO_PRESERVE_NAKADE 1
#define PO_ATTACK_LAST 1
#define PO_SAVE2 1
/* causes ladders, doesn't prevent thenm! */
#define PO_FORCE_LADDER 1

#define PO_RANDOM_PRUNE_SELF_ATARI 1
#define PO_RANDOM_PRUNE_SINGLE_DEAD 0
#define PO_RANDOM_PRUNE_CENTER 1
#define PO_RANDOM_PRUNE_SEKI 1
#define PO_RANDOM_PRUNE_EYE 1

#define PO_SAVE_NO_PATTERNS 1
#define PO_NEW_PATTERNS 1
#define PO_NO_PATTERN_SELF_ATARI 1
#define PO_NO_PATTERN_ONE_LIB 1
#define PO_MERCY 1

/* values for po_generate */
#define PO_CAPTURE_LAST_VAL 500
#define PO_PAT_VAL 24
#define PO_SAVE_VAL 200
#define PO_SAVE_CAP_VAL 50
#define PO_SAVE2_CAP_VAL 20
#define PO_SAVE_STONE_VAL 50
#define PO_NAKADE_VAL 300
#define PO_ATK2LIB_VAL 100
#define PO_PEEP_VAL 200
#define PO_LADDER_VAL 200
#define PO_ATTACK_VAL 50

/* this many visits means its a leaf - the win_rate will be either 0.0 or 1.0 */
#define UCT_LEAF 1000000000
#define PO_MAXMOVE (19 * 19 * 4)
#define PO_SZ 20
#define PO_BSIZE (PO_SZ * (PO_SZ + 1) + 2)
#define PO_PASS 0
#define PO_3_TO_8 (3*3*3*3*3*3*3*3)
#define PO_MAXLIBS 256

#define PO_BLACK 0
#define PO_WHITE 1
#define PO_EMPTY 2
#define PO_NO_COLOR 3

/* nodes per thread.  50k is enough for 5 minute 9x9 games or 30 minute 19x19 games
 */
#if USE_MPI

#ifdef MPIDEBUGOUTPUT
#define UCT_MAX_NODE 20000
#else
#define UCT_MAX_NODE (100000)
#endif

#else
/* number of parent nodes per thread */
#define UCT_MAX_NODE (40000)
#endif

/* /20 is enough for most games, but big 9x9 might need more */
#define UCT_MAX_STATE_NODE (UCT_MAX_NODE / 10)
#define UCT_MAX_SHARED_NODE (UCT_MAX_NODE / 10)

#if 0
#define UCT_STATE_HASH_BITS 14
#define UCT_STATE_HASH_SIZE (1 << UCT_STATE_HASH_BITS)
#define UCT_STATE_HASH_MASK (UCT_STATE_HASH_SIZE - 1)
#endif

#define UCT_HASH_BITS 15
#define UCT_HASH_SIZE (1 << UCT_HASH_BITS)
#define UCT_HASH_MASK (UCT_HASH_SIZE - 1)

#if UCT_MPI_ALLREDUCE
#define UCT_MPI_TARGETS 1
#else
#define UCT_MPI_TARGETS MPI_MAX_COMM
#endif
/*
 * TODO: design for huge reduction in memory usage
 *
 * parent has array of unions of
 * uct_child_ptr (used for children up to use_child_ptr - which is all visited, or for nodes with more than 250 total visits - to prevent rave overflow)
 * struct { uchar rave_visit, uchar rave_wins, short move }
 * use_child_ptr is same as use_children if parent visits < 256, otherwise it is the same as num_children
 *
 * average is only 4 used children per node.  current uses 362 * 28 bytes per node.  
 * new used 362 * 4 bytes plus 4 * 32 bytes
 * allocate and add a child node when progressive widening adds a child only, about 0.5 times per playout
 * add a next pointer to child array for free list
 * make child struct 32 bytes and force cache line alignment, one cache miss per chlid
 *
 * sorting children is much faster only 32 bit swap
 * same cache misses, about 1/child during search for best
 * fewer cache misses during rave update
 * progressive unpruning is not optional
 * when prune, lose the visit counts, or prune moves to just above use_children
 *
 * today 50K nodes uses 508 MB
 * new 50K nodes uses 74 MB plus 6 MB for children = 80 MB total
 */

/* 28 bytes per child */
struct uct_child {
	unsigned int visits;				/* times this move was played.  UCT_LEAF if this move is a leaf or deleted move */
	unsigned int wins;
	unsigned int last_visits;
	unsigned int last_wins;
	unsigned short move;				/* the move for this child */
#if USE_MPI
	unsigned int sent_mpi_visits[UCT_MPI_TARGETS];		/* TODO: with allreduce can reduce this to 1 */
	unsigned int sent_mpi_wins[UCT_MPI_TARGETS];
#endif
	float win_rate;						/* playouts won with this move, .9+0.01 per point or .1 - .01 per point for leaves, 0 for deleted mvoes */
	float child_val;					/* sqrt(1 / visits) */
	float rave_win_rate;				/* rave wins with this move */
#if UCT_RAVE_NEW_BETA
	float rave_beta;					/* silver rave formula result */
#endif
};

/*
 * much smaller design:
 * current size is 28 bytes per child, 840 child bytes, 7 bytes per move, 2534 bytes, 34 bytes other = 3408 bytes/position
 * rave_wins is indexed by point, not move index
 * init just one child when create parent, promote first several quickly - when better than any child, so child always has at least one visit
 * when promoting, break ties randomly

 *
 * most nodes are small number playouts and should have - total size is 602
 * unsigned char rave_wins per move (move is implied by index) = 362
 * int-ish *next, parent_val,  = 8
 * short mnum, ko = 4
 * unsigned char visits, tm+used, add_use = 4
 * 8 children - only track children that are actually in the tree - child become 32 bytes, add move to it, cache line align it = 256
 */

/* 
 * parent node holds all the information for a position.
 * nodes are private to each thread, no sharing
 */
struct uct_parent {
	__int64 hash;
	struct uct_parent *next;							/* next freee or next in the hash chain */
	struct uct_child child[UCT_MAX_CHILDREN];			/* moves that are promoted for UCT consideration */
	unsigned int mnum;									/* move number in game of this position */
	unsigned int visits;
	unsigned int last_visits;							/* visit count when I sent it last to other threads */
#if USE_MPI
	unsigned int sent_mpi_visits;						/* visits last time I sent this node to my partner node */
#endif
	float parent_val;
	unsigned short rave_plays[UCT_MAX_MOVES];			/* rave plays with this move */
	unsigned short rave_wins[UCT_MAX_MOVES];			/* rave wins with this move */
#ifdef UCTDEBUGOUTPUT
	unsigned char rave_prior[UCT_MAX_MOVES];			/* prior win rate (in 256ths) */
#endif
	unsigned char prior_mfgo[UCT_MAX_MOVES];			/* prior win rate from MFGO (in 256ths) */
	unsigned short move[UCT_MAX_MOVES];					/* all legal moves at this node */
	unsigned short num_moves;							/* number of generated moves */
	unsigned short num_children;						/* number of moves with child nodes and statistics */
	unsigned int add_use;								/* how many vists when we add the next move to try */
	short ko;											/* illegal ko point in this position.  multiple nodes for same position with different ko state */
	unsigned char tm;									/* tm and ko must match to match the node */
	char mfgo_called;									/* mfgo engine evaluated this node */
	char used;											/* node is in-use */
	char shared;						/* shared between threads and ranks */
};

#define PO_REASON_COUNT 16
#define PO_REASON_GAME 0
#define PO_REASON_UCT 1
#define PO_REASON_PO_CAPTURE_LAST 2
#define PO_REASON_PO_RANDOM 3
#define PO_REASON_PO_SAVE 4
#define PO_REASON_PO_PASS 5
#define PO_REASON_PO_PATTERN 6
#define PO_REASON_PO_LAST_PATTERN 7
#define PO_REASON_PO_CAPTURE 8
#define PO_REASON_PO_RANDOM_SELF_ATARI 9
#define PO_REASON_PO_ATTACK 10
#define PO_REASON_PO_NAKADE 11
#define PO_REASON_PO_PEEP 12
#define PO_REASON_PO_LADDER 13
#define PO_REASON_PO_SAVE2 14
#define PO_REASON_PO_ATTACK_NBR 15

/* 
 * smaller data (char and short) is faster 
 * about 32 KB for the board
 */
struct po_board {
	/* per point data */
	short p_g[PO_BSIZE];				/* group at this point (the board index of the first stone in this group), or PO_NONE */
	char  p_c[PO_BSIZE];				/* color */
	short p_next_g[PO_BSIZE];			/* next stone in group at this point (by point) */
	char  p_nbr_num[PO_BSIZE][2];		/* number of black and white neighbor groups (valid at empty points only) */
	short p_nbr[PO_BSIZE][2][4];		/* neighbor groups by color (empty points only) */
	char  p_empty_bits[PO_BSIZE];		/* one bit per direction if empty point in that direction, up, left, right, down is 1, 2, 4, 8 (use po_bit_offs1/2)*/
	short p_idx[PO_BSIZE];				/* index into empty point array */
	short p_pat[PO_BSIZE];				/* 3x3 pattern number matching here - 0 is all empty */
	char  p_first[PO_BSIZE];			/* the color to play first at this point during playout. PO_EMPTY if never played, PO_NOCOLOR if played but should be ignored for rave */
	short p_first_pat[PO_BSIZE];		/* the pattern that matched when this move was played */
#if PO_LIBS
	char  p_lib_bits[PO_BSIZE];			/* one bit per direction for a liberty of this group - each liberty only in one bit, across all stones */
#endif

	/* per group data - group number is the point of the first stone in its stone list */
	short g_libs[PO_BSIZE];				/* liberty count of this group */
	short g_size[PO_BSIZE];				/* stones in this group */
	short g_last_g[PO_BSIZE];			/* last stone in a group (by group) */

	/* per board data used by po_make_move */
	short boardsize;					/* 9, 19, etc */
	short passcount[2];					/* number of passes by color (and one for white for each handicap stone) */
	char  po_offs[16];					/* direction for an empty bit */
	char  po_onboard[PO_BSIZE];			/* on board bit mask for empty bits */
	short komi;							/* in points, white wins ties */
	short ko;							/* illegal ko point */
	short empty_idx[PO_BSIZE];			/* indexes of empty points in p_* arrays */
	short one_lib_num[2];				/* number of one liberty groups by color */
	short one_lib[2][PO_BSIZE];			/* group numbers of potential one liberty groups - these groups were reduced to one liberty at some time */
	short captured_num;					/* number of stones captured by last mvoe */
	short captured[PO_BSIZE];			/* points where stones were captured */
	short stones[2];					/* count of stones by color */
	short rave_moves;					/* last move number for collecting rave data */

	__int64 hash[PO_MAXMOVE];			/* zobrist hash of this position (mnum), pieces and ko point. not side to move for superko comparison */
	short terr[3];						/* count of territory by color */
	char  tm;							/* color to move */
	unsigned short mnum;				/* next move number */
	short empty_num;					/* number of empty points */
	short reason[PO_MAXMOVE];			/* why make this move */
	__int64 seed;						/* random seed */
	short moves[PO_MAXMOVE];			/* all moves played, the move last shifted 1 and the color */
};


struct po_board;
extern int po_pat_index[2][8];
extern unsigned char uct_po_patterns[2][PO_3_TO_8];
extern unsigned int uct_time_limit[PO_MAXMOVE];									/* how much time for this move, in secodns */
extern unsigned int uct_time_used[PO_MAXMOVE];

void po_init_all(void);
void po_pattern_init(void);
struct po_board *po_alloc(int komi, int size, __int64 seed);
void po_free(struct po_board *b);
void po_copy(struct po_board *dst, struct po_board *src);
void po_make_uct_move(struct po_board *b, int m, int c);
int po_playout(struct po_board *b, int rules, int handicap);
void po_init(struct po_board *b, int handicap, int komi, int size, __int64 seed);
short *po_getmoves(struct po_board *b, short *mnum);
void po_save_sgf(struct po_board *b, int handicap, char *name);
int po_to_po(sqr_t s, int size);
sqr_t po_from_po(int s, int size);
void po_uct_generate(struct po_board *b, struct uct_parent *node, int rules);
int po_score(struct po_board *b, int rules);
int po_superko(struct po_board *b);
void po_update_rave(struct po_board *b, struct uct_parent *t, int winner);
int po_random(struct po_board *b, int range);
int po_genmove(struct po_board *b);
void po_init_edge(int size);
int uct_make_child(struct uct_parent *node, int m, int slot);
unsigned int uct_best_rave(struct uct_parent *node);
