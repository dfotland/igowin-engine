/* Copyright 2007 David Fotland.  All rights reserved.
 * This source code is being distributed under a nondisclosure
 * agreement.  Do not distribute it without written permission
 * from David Fotland.  Do not show it to anyone not party to the
 * nondisclosure.  Maintain good security on any computers or
 * disks containing this source code.
 */


typedef int bool_t;

/* types of patterns */
#define LEARN_NUM_TYPES 3
#define LEARN_FUS 0
#define LEARN_JOS 1
#define LEARN_HB 2
#define LEARN_HJOS 3

/* one learned pattern.  expect to add several per game 
 * one pattern for every position seen more than once
 * and one pattern for each seen-once leaf.
 * nodes form a hash chain, with the head in the pattern hash table
 */

#define LEARN_GAME_BITS 20
#define LEARN_MNUM_BITS 9
#define LEARN_ORIENT_BITS 3

struct learn_node {
	unsigned __int64 key_next;	/* hash key (msb) and next node index (lsb) */
	unsigned int moves;			/* move list index (0 for no moves) */
	unsigned int orient:LEARN_ORIENT_BITS;			/* orientation */
	unsigned int next_move:LEARN_MNUM_BITS;			/* move number of the next move */
	unsigned int game:LEARN_GAME_BITS;			/* game index */
	unsigned short tenuki[2];	/* black and white tenuki from this position */
	unsigned short wins[3];		/* black and white and unknown wins from this position (count is sum of these three) */
	signed char strongest;		/* strongest player at this position */
	unsigned char size;			/* boardsize is size + 5 */
	unsigned char type;			/* type of entry */
};

#define LEARN_MAX_MOVES 360


/* game stored in the game file, with moves */
struct learn_game_file {
	char names[2][LEARN_MAX_STRING];			/* player names */
	char filename[LEARN_MAX_STRING];			/* file where game is saved (or NULL) */
	char event[LEARN_MAX_STRING];				/* name of event, round, etc */
	char result;								/* 1 - white wins, 2 - unknown, 0 - black wins */
	char boardsize;
	char strength[2];							/* strength of black, white */
	short year;									/* date played */
	char month;
	char day;
	short move_count;							/* how many moves in the game */
	unsigned int move[LEARN_MAX_MOVES/3];		/* the moves, 3 gomove_t per int */
};

struct learn_stats {
	int dupsize;
	int dupused;
	int games;
	int nodes;
	int multinodes;
	int moves;
	int types[LEARN_NUM_TYPES];
	int mem_used;
};

/*
 * init and read the database.  directory ends with a '/'
 * return FALSE if can't open the database
 */
bool_t learn_init(char *current_directory);

/*
 * init hash values for a new game
 */
void learn_init_game(int boardsize);
void learn_free(void);
void learn_get_stats(struct learn_stats *ls);

/*
 * add or delete a stone at a point to the hash function
 */
void learn_hash_move(int upptr);
void learn_hash_stone(sqr_t s, int c, int boardsize, int upptr);
sqr_t learn_hash_sym(sqr_t s, int h, int boardsize);

/* 
 * save or restore the hash values - used by monte carlo playouts
 */
void learn_get_hash(int upptr, unsigned __int64 *hash);
void learn_put_hash(int upptr, unsigned __int64 *hash);


/*
 * add game to the game database.  
 * add_current_game uses the position already in the engine.  add_game uses the moves in game
 */
bool_t learn_add_game(struct learn_game *game, int all);
bool_t learn_add_current_game(struct learn_game *game, int all);

/*
 * lookup a position and find out it's fuseki, joseki, etc
 */
void learn_lookup(unsigned __int64 *hash, struct learn_node **fuseki, struct learn_node *joseki[4], struct learn_node *side[4]);
int learn_is_fuseki(sqr_t s, int c);

/* get the game for the current position */
int learn_get_game(int color, struct learn_game *lg);

